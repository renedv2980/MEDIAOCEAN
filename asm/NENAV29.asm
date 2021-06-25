*          DATA SET NENAV29    AT LEVEL 002 AS OF 03/28/16                      
*PHASE T31829A                                                                  
NENAV29  TITLE 'NETWORK PATTERN UPLOAD'                                         
         PRINT NOGEN                                                            
                                                                                
*=====================================================================*         
*                                                                               
* HISTORY                                                                       
* -------                                                                       
*        WHEN                                                                   
* WHO   DDMMMYR LVL WHAT                                                        
* ----  ------- --- ----                                                        
* JBAS  03JAN16 001 INITIAL DEVELOPMENT - NET PATTERN UPLOAD                    
* JBAS  25MAR16 002                       ADD OPTICA ADD/CHANGE FLAGS           
*=====================================================================*         
                                                                                
SVRDEF   CSECT                                                                  
         LKSVR TYPE=U,WORKERKEY=NEPA,SEGMENT=Y,LINKIO=Y,               +        
               FACS=FACS,APPEND=Y,REQUEST=*,CODE=CODE,IDF=Y,           +        
               SYSPHASE=SYSPHASE,SYSTEM=NETSYSQ,                       +        
               BLOCKS=(B#WORKD,WORKD,B#SAVED,SAVED),RLEN=4000,         +        
               LOADFACSOFF=Y                                                    
                                                                                
WORKLNQ  EQU   CTABLNQ+HTABLNQ                                                  
CTABLNQ  EQU   (12*CTLNQ)+1                                                     
HTABLNQ  EQU   (12*HTLNQ)+1                                                     
CODE     NMOD1 WORKLNQ,**NN29**                                                 
         LR    RF,RC                                                            
         LARL  RA,GLOBALS                                                       
         USING GLOBALS,RA          RA=A(Global literals)                        
         LR    R5,R1                                                            
         USING LP_D,R5             R5=A(DDLINK parameter block)                 
         L     R7,LP_ARUNP                                                      
         USING RUNPARMD,R7         R7=A(RUNPARMS)                               
         SR    R6,R6                                                            
         ICM   R6,7,RUNPARUN                                                    
         USING RUNFACSD,R6         R6=A(RUNFACS)                                
         L     R9,LP_BLKS+((B#WORKD-1)*L'LP_BLKS)                               
         USING WORKD,R9            R9=A(Global w/s)                             
         L     R8,LP_BLKS+((B#SAVED-1)*L'LP_BLKS)                               
         USING SAVED,R8                                                         
         ST    RF,ACOMTAB                                                       
         AHI   RF,CTABLNQ                                                       
         ST    RF,AHIATAB                                                       
                                                                                
         MVC   RUNMODE,RUNPMODE    Extract DDLINK/RUNNER calling mode           
         MVC   ACOMFACS,RCOMFACS                                                
         MVC   AMASTC,RMASTC                                                    
                                                                                
         MVC   SVLPAGY,LP_AGY                                                   
                                                                                
         L     RF,ACOMFACS                                                      
         MVC   LINKIO,CLINKIO-COMFACSD(RF)                                      
         MVC   GETPROF,CGETPROF-COMFACSD(RF)                                    
         MVC   DATAMGR,CDATAMGR-COMFACSD(RF)                                    
         MVC   DATCON,CDATCON-COMFACSD(RF)                                      
         MVC   GETFACT,CGETFACT-COMFACSD(RF)                                    
         MVC   ALIOB,LP_ALIOB      Set A(LINKIO control block)                  
                                                                                
         L     RF,ACOMFACS                                                      
         L     RF,CCALLOV-COMFACSD(RF)                                          
         GOTOR (RF),DMCB,0,X'D9000AFE'                                          
         MVC   VTRPACK,0(R1)                                                    
         DROP  R6,R7,RB                                                         
                                                                                
         STM   R2,RB,LP_R2RB       Save registers for sub-routines              
         EJECT                                                                  
***********************************************************************         
*        INITIALIZE VARIABLES                                         *         
***********************************************************************         
                                                                                
         CLI   RUNMODE,RPRCWRKQ                                                 
         JNE   RUNREQ                                                           
                                                                                
         LA    R0,RQPAVALS                                                      
         LHI   R1,RQPAVALL                                                      
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         LA    R0,QVALS                                                         
         LHI   R1,L'QVALS                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
*        RUN UPLOAD REQUEST                                           *         
***********************************************************************         
                                                                                
RUNREQ   CLI   RUNMODE,RRUNREQQ    Test 'run request' mode                      
         JNE   YES                                                              
                                                                                
         GOTOR LP_APUTO,LP_D       Call DDLINK output processor                 
         OC    LP_ERROR,LP_ERROR   Test error number is set                     
         JZ    YES                 Exit back to DDLINK                          
         J     NO                                                               
         EJECT                                                                  
***********************************************************************         
*        NET PATTERN UPLOAD                                           *         
***********************************************************************         
                                                                                
PATUPL   LKREQ H,M#NEPATU,ROUTINE=UPLPAT,NEWREC=Y                               
                                                                                
Client   LKREQ F,D#PACLT,(D,B#SAVED,RQPACLT),CHAR,TEXT=(*,LITCLT),COL=*         
NetMedia LKREQ F,D#PANMD,(D,B#SAVED,RQPANMD),CHAR,TEXT=(*,LITNMD),COL=*         
Network  LKREQ F,D#PANET,(D,B#SAVED,RQPANET),CHAR,TEXT=(*,LITNET),COL=*         
Product  LKREQ F,D#PAPRO,(D,B#SAVED,RQPAPRO),CHAR,TEXT=(*,LITPRO),COL=*         
Daypart  LKREQ F,D#PADPT,(D,B#SAVED,RQPADPT),CHAR,TEXT=(*,LITDPT),COL=*         
Product  LKREQ F,D#PAPRD,(D,B#SAVED,RQPAPRD),CHAR,TEXT=(*,LITPRD),COL=*         
ProLen   LKREQ F,D#PAPRL,(D,B#SAVED,RQPAPRL),UBIN,TEXT=(*,LITPRL),COL=*         
PProduct LKREQ F,D#PAPPR,(D,B#SAVED,RQPAPPR),CHAR,TEXT=(*,LITPPR),COL=*         
PPartLen LKREQ F,D#PAPPL,(D,B#SAVED,RQPAPPL),UBIN,TEXT=(*,LITPPL),COL=*         
Refernce LKREQ F,D#PAREF,(D,B#SAVED,RQPAREF),UBIN,TEXT=(*,LITREF),COL=*         
Feed     LKREQ F,D#PAFED,(D,B#SAVED,RQPAFED),CHAR,TEXT=(*,LITFED),COL=*         
Desc     LKREQ F,D#PADSC,(D,B#SAVED,RQPADSC),CHAR,TEXT=(*,LITDSC),COL=*         
PerStrt  LKREQ F,D#PAPST,(D,B#SAVED,RQPAPST),BDAT,TEXT=(*,LITPST),COL=*         
PerEnd   LKREQ F,D#PAPEN,(D,B#SAVED,RQPAPEN),BDAT,TEXT=(*,LITPEN),COL=*         
UFNDate  LKREQ F,D#PAUFN,(D,B#SAVED,RQPAUFN),CHAR,TEXT=(*,LITUFN),COL=*         
StaTime  LKREQ F,D#PASTM,(D,B#SAVED,RQPASTM),CHAR,TEXT=(*,LITSTM),COL=*         
EndTime  LKREQ F,D#PAETM,(D,B#SAVED,RQPAETM),CHAR,TEXT=(*,LITETM),COL=*         
Nets     LKREQ F,D#PANETS,(I,B#SAVED,NETIND),CHAR,LIST=F,OLEN=4,       +        
               TEXT=(*,LITNETS),COL=*                                           
Coms     LKREQ F,D#PACOMS,(I,B#SAVED,COMIND),CHAR,LIST=F,SORT=NO,      +        
               OLEN=12,TEXT=(*,LITCOM),COL=*                                    
Pigs     LKREQ F,D#PAPIGS,(I,B#SAVED,PIGIND),CHAR,LIST=F,SORT=NO,      +        
               OLEN=12,TEXT=(*,LITPIG),COL=*                                    
Pcts     LKREQ F,D#PAPCTS,(I,B#SAVED,PCTIND),UBIN,LIST=F,SORT=NO,      +        
               OLEN=3,MAXLEN=3,TEXT=(*,LITPCT),COL=*                            
Rotation LKREQ F,D#PAROT,(D,B#SAVED,RQPAROT),CHAR,TEXT=(*,LITROT),COL=*         
CheckSum LKREQ F,D#PACKS,(D,B#SAVED,RQPACKS),HEXD,TEXT=(*,LITCKS),COL=*         
Cmts     LKREQ F,D#PACMTS,(I,B#SAVED,CMTIND),CHAR,LIST=F,SORT=NO,      +        
               ,OLEN=52,TEXT=(*,LITCMT),COL=*                                   
HDys     LKREQ F,D#PAHDYS,(I,B#SAVED,HDYIND),CHAR,LIST=F,SORT=NO,      +        
               OLEN=10,TEXT=(*,LITHDY),COL=*                                    
HDts     LKREQ F,D#PAHDTS,(I,B#SAVED,HDTIND),BDAT,LIST=F,SORT=NO,      +        
               OLEN=3,TEXT=(*,LITHDT),COL=*                                     
HSTs     LKREQ F,D#PAHSTS,(I,B#SAVED,HSTIND),CHAR,LIST=F,SORT=NO,      +        
               OLEN=5,TEXT=(*,LITHST),COL=*                                     
HETs     LKREQ F,D#PAHETS,(I,B#SAVED,HETIND),CHAR,LIST=F,SORT=NO,      +        
               OLEN=5,TEXT=(*,LITHET),COL=*                                     
UpdApp   LKREQ F,D#PAUAP,(D,B#SAVED,RQPAUAP),CHAR,TEXT=(*,LITUAP),COL=*         
Action   LKREQ F,D#PAACT,(D,B#SAVED,RQPAACT),CHAR,TEXT=(*,LITACT),COL=*         
         LKMAP E                                                                
                                                                                
LITCLT   DC    C'Client Code'                                                   
LITNMD   DC    C'Network Media'                                                 
LITNET   DC    C'Network'                                                       
LITPRO   DC    C'Program'                                                       
LITDPT   DC    C'Daypart Code'                                                  
LITPRD   DC    C'Product'                                                       
LITPRL   DC    C'Product Length'                                                
LITPPR   DC    C'Product Partner'                                               
LITPPL   DC    C'Product Partner Length'                                        
LITREF   DC    C'Reference Number'                                              
LITFED   DC    C'Feed'                                                          
LITDSC   DC    C'Description'                                                   
LITPST   DC    C'Period Start Date'                                             
LITPEN   DC    C'Period End Date'                                               
LITUFN   DC    C'UFN Date?'                                                     
LITSTM   DC    C'Start Time'                                                    
LITETM   DC    C'End Time'                                                      
LITNETS  DC    C'Networks'                                                      
LITCOM   DC    C'Commercial(s)'                                                 
LITPIG   DC    C'Piggyback Commercial(s)'                                       
LITPCT   DC    C'Percentage(s)'                                                 
LITROT   DC    C'Rotation'                                                      
LITCKS   DC    C'Check Sum'                                                     
LITCMT   DC    C'Comment(s)'                                                    
LITHDY   DC    C'Hiatus Day(s)'                                                 
LITHDT   DC    C'Hiatus Date(s)'                                                
LITHST   DC    C'Hiatus Start Time(s)'                                          
LITHET   DC    C'Hiatus End Time(s)'                                            
LITUAP   DC    C'Updating Application'                                          
LITACT   DC    C'Action'                                                        
                                                                                
         EJECT                                                                  
***********************************************************************         
*        PATTERN ADD/CHANGE/DELETE/RESTORE UPLOAD                     *         
***********************************************************************         
                                                                                
         USING LIOB,R5                                                          
UPLPAT   L     R5,ALIOB                                                         
         GOTO1 LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTMAP',M#NEPATU)               
                                                                                
         BRAS  RE,VALACT           ENSURE ACTION IS PROVIDED                    
         JNE   UP40                AND IS VALID                                 
                                                                                
         LA    R0,SAVED            CLEAR LOCAL STORAGE                          
         LHI   R1,LOCALL                                                        
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         L     R0,ACOMTAB          CLEAR COMMERCIAL/HIATUS TABLES               
         LHI   R1,WORKLNQ                                                       
         MVCL  R0,RE                                                            
         L     RE,AHIATAB                                                       
         MVI   0(RE),X'FF'                                                      
                                                                                
         GOTOR FMTLIST,DMCB,NETIND FORMAT REQUEST LISTS                         
         GOTOR FMTLIST,DMCB,COMIND                                              
         GOTOR FMTLIST,DMCB,PIGIND                                              
         GOTOR FMTLIST,DMCB,PCTIND                                              
         GOTOR FMTLIST,DMCB,CMTIND                                              
         GOTOR FMTLIST,DMCB,HDYIND                                              
         GOTOR FMTLIST,DMCB,HDTIND                                              
         GOTOR FMTLIST,DMCB,HSTIND                                              
         GOTOR FMTLIST,DMCB,HETIND                                              
                                                                                
         GOTOR SETLEN,DMCB,ROTIND,RQPAROT,L'RQPAROT                             
                                                                                
         LA    R3,IOKEY            R3=A(IOKEY)                                  
                                                                                
         CLI   RQPAACT,ACTADD      IF ACTION IS ADD                             
         JNE   UP10                                                             
         BRAS  RE,ADDPAT           HANDLE ADDITION OF PATTERN                   
         JNE   UP40                                                             
                                                                                
UP10     CLI   RQPAACT,ACTCHA      IF ACTION IS CHANGE                          
         JNE   UP20                                                             
         BRAS  RE,CHGPAT           HANDLE CHANGE OF PATTERN                     
         JNE   UP40                                                             
                                                                                
UP20     CLI   RQPAACT,ACTDEL      IF ACTION IS DELETE                          
         JNE   UP30                                                             
         BRAS  RE,DELPAT           HANDLE DELETE OF PATTERN                     
         JNE   UP40                                                             
                                                                                
UP30     CLI   RQPAACT,ACTRES      IF ACTION IS RESTORE                         
         JNE   UP40                                                             
         BRAS  RE,RESPAT           HANDLE RESTORE OF PATTERN                    
                                                                                
UP40     GOTO1 LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTRAW',D#PACLT),      +        
               ('LD_CHARQ',RQPACLT),(L'RQPACLT,0)                               
                                                                                
         CLI   RQPANMD,0                                                        
         JE    UP50                                                             
         GOTO1 LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTRAW',D#PANMD),      +        
               ('LD_CHARQ',RQPANMD),(L'RQPANMD,0)                               
                                                                                
UP50     OC    RQPANET,RQPANET                                                  
         JZ    UP60                                                             
         GOTO1 LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTRAW',D#PANET),      +        
               ('LD_CHARQ',RQPANET),(L'RQPANET,0)                               
                                                                                
UP60     OC    RQPAPRO,RQPAPRO                                                  
         JZ    UP70                                                             
         GOTO1 LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTRAW',D#PAPRO),      +        
               ('LD_CHARQ',RQPAPRO),(L'RQPAPRO,0)                               
                                                                                
UP70     OC    RQPADPT,RQPADPT                                                  
         JZ    UP80                                                             
         GOTO1 LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTRAW',D#PADPT),      +        
               ('LD_CHARQ',RQPADPT),(L'RQPADPT,0)                               
                                                                                
UP80     OC    RQPAPRD,RQPAPRD                                                  
         JZ    UP90                                                             
         GOTO1 LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTRAW',D#PAPRD),      +        
               ('LD_CHARQ',RQPAPRD),(L'RQPAPRD,0)                               
                                                                                
UP90     OC    RQPAPRL,RQPAPRL                                                  
         JZ    UP100                                                            
         GOTO1 LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTRAW',D#PAPRL),      +        
               ('LD_UBINQ',RQPAPRL),(L'RQPAPRL,0)                               
                                                                                
UP100    OC    RQPAPPR,RQPAPPR                                                  
         JZ    UP110                                                            
         GOTO1 LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTRAW',D#PAPPR),      +        
               ('LD_CHARQ',RQPAPPR),(L'RQPAPPR,0)                               
                                                                                
UP110    OC    RQPAPPL,RQPAPPL                                                  
         JZ    UP120                                                            
         GOTO1 LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTRAW',D#PAPPL),      +        
               ('LD_UBINQ',RQPAPPL),(L'RQPAPPL,0)                               
                                                                                
UP120    OC    RQPAREF,RQPAREF                                                  
         JZ    UP140                                                            
         CLI   RQPAACT,ACTADD                                                   
         JNE   UP130                                                            
         OC    ERROR,ERROR                                                      
         JNZ   UP140                                                            
UP130    GOTO1 LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTRAW',D#PAREF),      +        
               ('LD_UBINQ',RQPAREF),(L'RQPAREF,0)                               
                                                                                
UP140    OC    RQPAFED,RQPAFED                                                  
         JZ    UP150                                                            
         GOTO1 LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTRAW',D#PAFED),      +        
               ('LD_CHARQ',RQPAFED),(L'RQPAFED,0)                               
                                                                                
UP150    CLI   NETIND,0                                                         
         JE    UP160                                                            
         GOTO1 LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTRAW',D#PANETS),     +        
               ('LD_CHARQ',=C'$'),(1,0)                                         
                                                                                
UP160    OC    RQPACKS,RQPACKS                                                  
         JZ    UP170                                                            
         GOTO1 VHEXOUT,DMCB,RQPACKS,DUB2,L'RQPACKS,0                            
         GOTO1 LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTRAW',D#PACKS),      +        
               ('LD_CHARQ',DUB2),(8,0)                                          
                                                                                
UP170    OC    ERROR,ERROR                                                      
         JZ    YES                                                              
         GOTO1 LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTRAW',254),          +        
               ('LD_UBINQ',ERRFLD),(L'ERRFLD,0)                                 
         CLI   LIOBMSYS,0                                                       
         JNE   UP180                                                            
         GOTO1 LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTRAW',D#UPLERR),     +        
               ('LD_CHARQ',ERRTXT),(L'ERRTXT,0)                                 
         J     YES                                                              
UP180    GOTO1 LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTERR',D#UPLERR),     +        
               ERROR,0                                                          
         J     YES                                                              
                                                                                
REQEND   LKREQ X                                                                
         EJECT                                                                  
***********************************************************************         
*        ROUTINE ASSERTS THAT ACTION IS PROVIDED AND IS VALID         *         
***********************************************************************         
                                                                                
VALACT   NTR1  BASE=*,LABEL=*                                                   
         MVI   ERRFLD,D#PAACT                                                   
         CLI   RQPAACT,0           ENSURE ACTION IS PROVIDED                    
         JE    ERR00001                                                         
         CLI   RQPAACT,ACTADD      AND IS ADD                                   
         JE    YES                                                              
         CLI   RQPAACT,ACTCHA      CHANGE                                       
         JE    YES                                                              
         CLI   RQPAACT,ACTDEL      DELETE                                       
         JE    YES                                                              
         CLI   RQPAACT,ACTRES      OR RESTORE                                   
         JNE   ERR00002                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
*        ROUTINE FORMATS LIST                                         *         
*        ON ENTRY ... P1=A(LIST FIELD)                                *         
***********************************************************************         
                                                                                
FMTLIST  NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)                                                         
         OC    0(4,R2),0(R2)                                                    
         JZ    XIT                                                              
                                                                                
         ZICM  RF,1(R2),3                                                       
         LHI   RE,9                                                             
         AR    RF,RE                                                            
         MVC   0(1,R2),0(RF)                                                    
         LA    RF,1(RF)                                                         
         STCM  RF,7,1(R2)                                                       
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO SET LENGTH OF INPUT IN PROVIDED FIELD             *         
*        ON ENTRY ... P1=A(LENGTH FIELD)                              *         
*                     P2=A(PROVIDED FIELD)                            *         
*                     P3=L'PROVIDED FIELD                             *         
***********************************************************************         
                                                                                
SETLEN   NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)            R2=A(LENGTH FIELD)                           
         ZIC   R3,11(R1)           R3=L'PROVIDED FIELD                          
                                                                                
         L     RE,4(R1)            RE=A(PROVIDED FIELD)                         
                                                                                
         LR    RF,R3               COPY LENGTH                                  
         LA    R4,0(RF,RE)         POINT R4 TO LAST CHAR OF FIELD               
         SHI   R4,1                                                             
                                                                                
SLEN10   CLI   0(R4),C' '          FINDS LAST NON-BLANK CHAR                    
         JH    SLEN20              THAT'S THE NEW LENGTH                        
         AHI   R4,-1               PREVIOUS CHAR                                
         AHI   RF,-1               DECREMENT LENGTH                             
         JZ    SLEN20                                                           
         J     SLEN10                                                           
                                                                                
SLEN20   STC   RF,0(R2)                                                         
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        ROUTINE HANDLES ADDITION OF PATTERN RECORD                   *         
*        ON ENTRY ... R3 = A(IOKEY)                                   *         
***********************************************************************         
                                                                                
ADDPAT   NTR1  BASE=*,LABEL=*                                                   
         MVI   ERRFLD,D#PAREF                                                   
         OC    RQPAREF,RQPAREF     ASSERT THAT REFERENCE NUMBER                 
         JNZ   ERR00006            IS NOT PROVIDED                              
                                                                                
         MVI   ERRFLD,D#PACKS                                                   
         OC    RQPACKS,RQPACKS     ASSERT THAT CHECKSUM IS NOT                  
         JNZ   ERR00006            PROVIDED                                     
                                                                                
         MVI   ERRFLD,D#PACOMS                                                  
         ZIC   R0,COMIND           ASSERT NO COMMERCIALS ARE BEING              
         ZICM  R1,COMS,3           DELETED                                      
APAT10   CLC   =CL12'*',0(R1)                                                   
         JE    ERR00002                                                         
         LA    R1,12(R1)                                                        
         JCT   R0,APAT10                                                        
                                                                                
         BRAS  RE,BLDTABS          BUILD COMMERCIALS AND HIATUS TABLES          
                                                                                
         BRAS  RE,VPATAC           VALIDATE PATTERN RECORD                      
         JNE   NO                  FOR ADD/CHANGE                               
                                                                                
         USING NPTXKEY,R3                                                       
         BRAS  RE,INIPATKY         INITIALIZE PATTERN KEY                       
         MVI   NPTXR3F,X'A0'       AND READ FOR IT                              
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOXSPDIR+IO3'                            
         CLC   IOKEY(NPTXR3F-NPTXKEY),IOKEYSAV                                  
         JE    APAT20                                                           
                                                                                
         MVC   IOKEY,IOKEYSAV      IF NO PATTERN FOUND                          
         MVC   NPTXR3F,=X'FFFFFE'  ASSIGN REFERENCE NUMBER 1                    
         MVI   RQPAREF+2,1                                                      
         J     APAT30                                                           
                                                                                
APAT20   ZICM  R1,NPTXR3F,3        IF PATTERN IS FOUND                          
         X     R1,=XL4'00FFFFFF'   ASSIGN NEXT REFERENCE NUMBER                 
         AHI   R1,1                                                             
         STCM  R1,7,RQPAREF                                                     
         MVC   IOKEY,IOKEYSAV                                                   
         X     R1,=XL4'00FFFFFF'                                                
         STCM  R1,7,NPTXR3F                                                     
         DROP  R3                                                               
                                                                                
APAT30   MVC   SVTXKEY,IOKEY                                                    
                                                                                
         BRAS  RE,VALNETS          VALIDATE NETWORKS                            
         JNE   NO                                                               
                                                                                
         BRAS  RE,GETSEQ           GET SEQUENCE NUMBER                          
                                                                                
         USING NPTXKEY,R4                                                       
         L     R4,AIO3                                                          
         XC    0(255,R4),0(R4)     INITIALIZE RECORD                            
         MVC   NPTXKEY,SVTXKEY     WITH KEY                                     
         MVI   L'NPTXKEY+1(R4),41  AND LENGTH                                   
                                                                                
         BRAS  RE,BLDREC           BUILD PATTERN RECORD                         
         JE    *+6                                                              
         DC    H'00'                                                            
         DROP  R4                                                               
                                                                                
         USING NPTDATA,R4                                                       
         CLI   RQPAUAP,C'O'                                                     
         JNE   APAT35                                                           
         L     R4,AIO3                                                          
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         OI    NPTSTAT,NPTOPADD                                                 
         DROP  R4                                                               
                                                                                
APAT35   GOTOR ADDACT,DMCB,AIO3                                                 
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOADDREC+IOXSPFIL+IO3'                        
                                                                                
         L     R4,AIO3             SET NEW CHECKSUM                             
         GOTOR (#SETCKS,ASETCKS),DMCB,AIO3,L'NPTXKEY(R4),RQPACKS                
                                                                                
         USING NPTXKEY,R3                                                       
         ZICM  R2,NETIND,1                                                      
         JZ    APAT50                                                           
         ZICM  R4,NETS,3                                                        
         MVC   IOKEY,SVTXKEY                                                    
         MVI   NPTXPSSV,C'$'                                                    
         MVC   NPTXDKAD,IOWORK+4                                                
APAT40   MVC   NPTXNET,0(R4)                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOADD+IOXSPDIR+IO3'                           
         LA    R4,4(R4)                                                         
         JCT   R2,APAT40                                                        
                                                                                
APAT50   XC    IOKEY,IOKEY                                                      
         MVC   NPTPXID,=X'0AE1'                                                 
         MVC   NPTPXAM,QMEDX                                                    
         MVC   NPTPXCLT,QCLTX                                                   
         MVC   NPTPXS3Q,SEQNUM                                                  
         MVC   NPTXDKAD,IOWORK+4                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOADD+IOXSPDIR+IO3'                           
         J     XIT                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        ROUTINE GETS SEQUENCE NUMBER FOR PATTERN                     *         
*        ON ENTRY ... R3 = A(IOKEY)                                   *         
***********************************************************************         
                                                                                
GETSEQ   NTR1  BASE=*,LABEL=*                                                   
         USING NPTXKEY,R3                                                       
         XC    IOKEY,IOKEY                                                      
         MVC   NPTXKID,=X'0A61'    BUILD PATTERN SEQUENCE KEY                   
         MVC   NPTXAM,QMEDX        WITH MEDIA                                   
         MVC   NPTXCLT,QCLTX       AND CLIENT                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOXSPDIR+IO4'                            
         CLC   NPTXKEY,IOKEYSAV                                                 
         JNE   GSEQ10                                                           
         DROP  R3                                                               
                                                                                
***********************************************************************         
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOXSPFIL+IO4'                        
         MVI   DATADISP+1,42                                                    
                                                                                
         USING NPTDATA,R4                                                       
         L     R4,AIO4             IF IT EXISTS                                 
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVC   SEQNUM,NPTS3QNO     SAVE THIS PATTERN'S SEQUENCE NUMBER          
         ZICM  RE,NPTS3QNO,3                                                    
         AHI   RE,1                                                             
         STCM  RE,7,NPTS3QNO       BUMP UP NEXT SEQUENCE NUMBER                 
                                                                                
         ZICM  RE,NPTR3FNO,3       ALSO BUMP UP NEXT REFERENCE NUMBER           
         AHI   RE,1                (ALTHOUGH THIS DOES NOT SEEM                 
         STCM  RE,7,NPTR3FNO       IMPORTANT)                                   
         DROP  R4                                                               
                                                                                
         GOTOR UPDACT,DMCB,AIO4                                                 
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOXSPFIL+IO4'                        
         J     XIT                                                              
                                                                                
***********************************************************************         
                                                                                
         USING NPTXKEY,R4                                                       
GSEQ10   L     R4,AIO4             IF IT DOESN'T EXIST                          
         XC    0(255,R4),0(R4)     INITIALIZE RECORD                            
         MVC   NPTXKEY,IOKEYSAV    WITH KEY                                     
         MVI   L'NPTXKEY+1(R4),41  AND LENGTH                                   
                                                                                
         USING NPTDATA,R4          BUILD PATTERN DATA ELEMENT                   
         LA    R4,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   NPTDATA,X'10'                                                    
         MVI   NPTDTALN,NPTDTAX-NPTDATA                                         
         MVC   NPTDESC,=CL24'PATTERN SEQ RECORD'                                
         MVI   NPTS3QNO+2,2                                                     
         GOTO1 VHELLO,DMCB,(C'P',=C'XSPFIL'),AIO4,(R4),0                        
         DROP  R4                                                               
                                                                                
         GOTOR ADDACT,DMCB,AIO4                                                 
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOADDREC+IOXSPFIL+IO4'                        
                                                                                
         MVC   SEQNUM,=XL3'1'                                                   
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE HANDLES CHANGE OF PATTERN RECORD                     *         
*        ON ENTRY ... R3 = A(IOKEY)                                   *         
***********************************************************************         
                                                                                
CHGPAT   NTR1  BASE=*,LABEL=*                                                   
         MVI   ERRFLD,D#PAREF                                                   
         OC    RQPAREF,RQPAREF     ASSERT THAT REFERNCE NUMBER                  
         JZ    ERR00001            IS PROVIDED                                  
                                                                                
         MVI   ERRFLD,D#PACKS                                                   
         OC    RQPACKS,RQPACKS     ASSERT THAT CHECKSUM IS PROVIDED             
         JZ    ERR00001                                                         
                                                                                
         BRAS  RE,BLDTABS          BUILD COMMERCIALS AND HIATUS TABLES          
                                                                                
         BRAS  RE,VPATAC           VALIDATE PATTERN RECORD FOR                  
         JNE   NO                  FOR ADD/CHANGE                               
                                                                                
         BRAS  RE,VALREF           VALIDATE REFERENCE NUMBER                    
         JNE   NO                                                               
                                                                                
         USING NPTDATA,R4                                                       
         L     R4,AIO3                                                          
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         TM    NPTSTAT,NPTS_DEL    ENSURE PATTERN IS NOT DELETED                
         JO    ERR00053                                                         
         DROP  R4                                                               
                                                                                
         BRAS  RE,VALNETS          VALIDATE NETWORKS                            
         JNE   NO                                                               
                                                                                
         BRAS  RE,VALCKS           VALIDATE CHECKSUM                            
         JNE   NO                                                               
                                                                                
***********************************************************************         
                                                                                
         USING NPTDATA,R4                                                       
         L     R4,AIO3                                                          
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   OSEQNUM,NPTS3QNO                                                 
         MVC   SEQNUM,NPTS3QNO                                                  
         DROP  R4                                                               
                                                                                
         GOTO1 VHELLO,DMCB,(C'D',=C'XSPFIL'),(X'10',AIO3),0                     
         GOTO1 VHELLO,DMCB,(C'D',=C'XSPFIL'),(X'30',AIO3),0                     
         GOTO1 VHELLO,DMCB,(C'D',=C'XSPFIL'),(X'32',AIO3),0                     
         GOTO1 VHELLO,DMCB,(C'D',=C'XSPFIL'),(X'34',AIO3),0                     
         GOTO1 VHELLO,DMCB,(C'D',=C'XSPFIL'),(X'36',AIO3),0                     
         GOTO1 VHELLO,DMCB,(C'D',=C'XSPFIL'),(X'40',AIO3),0                     
         GOTO1 VHELLO,DMCB,(C'D',=C'XSPFIL'),(X'5C',AIO3),0                     
                                                                                
         L     R4,AIO3                                                          
         MVI   ELCODE,X'5B'        MARK ALL NETWORK ELEMENTS AS                 
         BRAS  RE,GETEL            DELETED                                      
         J     *+8                                                              
CPAT10   BRAS  RE,NEXTEL                                                        
         JNE   CPAT20                                                           
         OI    6(R4),X'80'                                                      
         J     CPAT10                                                           
                                                                                
CPAT20   BRAS  RE,BLDREC           BUILD PATTERN RECORD                         
         JNE   NO                                                               
                                                                                
         USING NPTDATA,R4                                                       
         CLI   RQPAUAP,C'O'                                                     
         JNE   CPAT25                                                           
         L     R4,AIO3                                                          
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         OI    NPTSTAT,NPTOPCHG                                                 
         DROP  R4                                                               
                                                                                
***********************************************************************         
                                                                                
CPAT25   BAS   RE,CHKSIG           IF SIGNIFICANT CHANGE MADE                   
         JNE   CPAT30                                                           
                                                                                
         BRAS  RE,GETSEQ           GET NEXT SEQUENCE NUMBER                     
                                                                                
         USING NPTXKEY,R4                                                       
         L     R4,AIO2             RE-ADD OLD PATTERN RECORD                    
         MVC   NPTXOR3G,NPTXR3F    WITH OLD REFERNCE NUMBER                     
         MVC   NPTXR3F,SEQNUM      NEW REFERENCE NUMBER                         
         GOTOR UPDACT,DMCB,AIO2    AND UPDATED ACTIVITY ELEMENT                 
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOADDREC+IOXSPFIL+IO2'                        
         DROP  R4                                                               
                                                                                
         L     R0,IOWORK+4                                                      
                                                                                
         USING NPTXKEY,R3                                                       
         XC    IOKEY,IOKEY         UPDATE OLD PASSIVE (WITH OLD                 
         MVC   NPTPXID,=X'0AE1'    REFERENCE NUMBER) WITH NEW DISK              
         MVC   NPTPXAM,QMEDX       ADDRESS                                      
         MVC   NPTPXCLT,QCLTX                                                   
         MVC   NPTPXS3Q,OSEQNUM                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IORDUP+IOXSPDIR+IO4'                          
         JE    *+6                                                              
         DC    H'00'                                                            
         L     R2,IOKEY+36                                                      
         ST    R0,IOKEY+36                                                      
         GOTOR (#IOEXEC,AIOEXEC),'IOWRITE+IOXSPDIR+IO4'                         
         DROP  R3                                                               
                                                                                
         USING NPTXKEY,R3                                                       
         XC    IOKEY,IOKEY                                                      
         MVC   NPTPXID,=X'0AE1'    ADD NEW PASSIVE (WITH NEW SEQUENCE           
         MVC   NPTPXAM,QMEDX       NUMBER) WITH OLD DISK ADDRESS                
         MVC   NPTPXCLT,QCLTX                                                   
         MVC   NPTPXS3Q,SEQNUM                                                  
         ST    R2,IOKEY+36                                                      
         GOTOR (#IOEXEC,AIOEXEC),'IOADD+IOXSPDIR+IO4'                           
         DROP  R3                                                               
                                                                                
         USING NPTDATA,R4                                                       
         L     R4,AIO3                                                          
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'               SAVE NEW SEQUENCE NUMBER                     
         MVC   NPTS3QNO,SEQNUM     INTO PATTERN                                 
         DROP  R4                                                               
                                                                                
***********************************************************************         
                                                                                
CPAT30   GOTOR UPDACT,DMCB,AIO3                                                 
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOXSPFIL+IO3'                        
                                                                                
         L     R4,AIO3             SET NEW CHECKSUM                             
         GOTOR (#SETCKS,ASETCKS),DMCB,AIO3,L'NPTXKEY(R4),RQPACKS                
                                                                                
***********************************************************************         
                                                                                
         ZICM  R2,NETIND,1                                                      
         JZ    YES                                                              
         ZICM  R6,NETS,3                                                        
                                                                                
CPAT40   TM    PROSTAT,PSSIGCHG                                                 
         JO    CPAT60                                                           
                                                                                
         L     R4,AIO2                                                          
         MVI   ELCODE,X'5B'                                                     
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
CPAT50   BRAS  RE,NEXTEL                                                        
         JNE   CPAT60                                                           
         CLC   0(4,R6),2(R4)                                                    
         JNE   CPAT50                                                           
         J     CPAT70                                                           
                                                                                
         USING NPTXKEY,R3                                                       
CPAT60   MVC   IOKEY,SVTXKEY                                                    
         MVI   NPTXPSSV,C'$'                                                    
         MVC   NPTXDKAD,IOWORK+4                                                
         MVC   NPTXNET,0(R6)                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOADD+IOXSPDIR+IO3'                           
         DROP  R3                                                               
                                                                                
CPAT70   LA    R6,4(R6)                                                         
         JCT   R2,CPAT40                                                        
         J     YES                                                              
***********************************************************************         
*        ROUTINE CHECKS FOR SIGIFICANT CHANGE                         *         
*        ON ENTRY ... AIO3 = A(UPDATED PATTERN RECORD)                *         
*                     R3   = A(IOKEY)                                 *         
***********************************************************************         
                                                                                
         USING NPTXKEY,R3                                                       
CHKSIG   NTR1  BASE=*,LABEL=*                                                   
         MVC   NPTXKEY,SVTXKEY     GET EXISTING PATTERN INTO AIO2               
         GOTOR (#IOEXEC,AIOEXEC),'IOHIUP+IOXSPDIR+IO2'                          
         CLC   NPTXKEY(NPTXR3F-NPTXKEY),IOKEYSAV                                
         JE    *+6                                                              
         DC    H'00'                                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOXSPFIL+IO2'                        
         MVI   DATADISP+1,42                                                    
                                                                                
***********************************************************************         
                                                                                
         GOTO1 SCGETEL,DMCB,(X'10',0)                                           
                                                                                
         USING NPTDATA,R2                                                       
         CLC   NPTSTART,NPTSTART-NPTDATA(R4)                                    
         JNE   SCYES               DATE CHANGE IS SIGNIFICANT                   
         CLC   NPTEND,NPTEND-NPTDATA(R4)                                        
         JNE   SCYES                                                            
                                                                                
         CLC   NPTSTIM,NPTSTIM-NPTDATA(R4)                                      
         JNE   SCYES               TIME CHANGE IS SIGNIFICANT                   
         CLC   NPTETIM,NPTETIM-NPTDATA(R4)                                      
         JNE   SCYES                                                            
                                                                                
         LA    R6,NPTSTAT                                                       
         DROP  R2                                                               
                                                                                
***********************************************************************         
                                                                                
         GOTO1 SCGETEL,DMCB,(X'30',0)                                           
                                                                                
         USING NPTCMLEL,R2                                                      
         TM    PROSTAT,PSADID      IF COMMERCIALS ARE ENTERED AS AD-ID          
         JZ    SC30                                                             
         TM    0(R6),NPTS_ADID     BUT WEREN'T PREVIOUSLY ...                   
         JO    SC30                                                             
         LLC   R0,NPTCMLLN                                                      
         SRDL  R0,4                R0=(# OF COMML PAIRS TO TRANSLATE)           
         LA    R6,NPTCML           R4=A(COMMERCIAL PAIR)                        
                                                                                
SC10     MVC   WORK(12),SPACES                                                  
         MVC   WORK(8),0(R6)                                                    
         GOTO1 VTRPACK,DMCB,(C'P',WORK),0(R6)                                   
         OC    8(8,R6),8(R6)                                                    
         JZ    SC20                                                             
         MVC   WORK(12),SPACES                                                  
         MVC   WORK(8),8(R6)                                                    
         GOTO1 VTRPACK,DMCB,(C'P',WORK),8(R6)                                   
         LA    R6,16(R6)                                                        
SC20     JCT   R0,SC10                                                          
                                                                                
         ZIC   RE,NPTCMLLN         COMMERCIAL(S) CHANGE IS                      
         BCTR  RE,0                SIGNIFICANT                                  
         EX    RE,*+8                                                           
         J     *+10                                                             
         CLC   NPTCMLEL,0(R4)                                                   
         JNE   SCYES                                                            
         DROP  R2                                                               
                                                                                
***********************************************************************         
                                                                                
SC30     L     R4,AIO2                                                          
         MVI   ELCODE,X'32'                                                     
         BRAS  RE,GETEL            PERCENTAGE CHANGE IS SIGNIFICANT             
         JE    SC40                                                             
         L     R4,AIO2                                                          
         MVI   ELCODE,X'36'                                                     
         BRAS  RE,GETEL                                                         
         JE    SC40                                                             
         L     R4,AIO2                                                          
         MVI   ELCODE,X'34'                                                     
         BRAS  RE,GETEL                                                         
         JE    SC40                                                             
         XR    R4,R4                                                            
                                                                                
SC40     LR    R2,R4                                                            
                                                                                
         L     R4,AIO3                                                          
         MVI   ELCODE,X'32'                                                     
         BRAS  RE,GETEL                                                         
         JE    SC50                                                             
         L     R4,AIO3                                                          
         MVI   ELCODE,X'36'                                                     
         BRAS  RE,GETEL                                                         
         JE    SC50                                                             
         L     R4,AIO3                                                          
         MVI   ELCODE,X'34'                                                     
         BRAS  RE,GETEL                                                         
         JE    SC50                                                             
         XR    R4,R4                                                            
                                                                                
SC50     CR    R2,R4                                                            
         JE    NO                                                               
         LTR   R2,R2                                                            
         JZ    SCYES                                                            
         LTR   R4,R4                                                            
         JZ    SCYES                                                            
                                                                                
         ZIC   RE,1(R2)            PERCENTAGE/ROTATION CHANGE IS                
         BCTR  RE,0                SIGNIFICANT                                  
         EX    RE,*+8                                                           
         J     *+10                                                             
         CLC   0(0,R2),0(R4)                                                    
         JE    NO                                                               
                                                                                
***********************************************************************         
                                                                                
SCYES    OI    PROSTAT,PSSIGCHG    SET SIGNIFICANT CHANGE INDICTOR              
         J     YES                                                              
                                                                                
***********************************************************************         
*        ROUTINE RETURNS A(ELEMENT) FROM ORIGINAL RECORD IN R2        *         
*        AND A(ELEMENT) FROM UPDATED RECORD IN R4                     *         
*        ON ENTRY ... P0 BYTE 0 = ELEMENT CODE                        *         
*                     AIO2      = A(ORIGINAL RECORD)                  *         
*                     AIO3      = A(UPDATED RECORD)                   *         
***********************************************************************         
                                                                                
SCGETEL  NTR1                                                                   
         MVC   BYTE4,0(R1)                                                      
                                                                                
         XR    R2,R2                                                            
         XR    R4,R4                                                            
                                                                                
         L     R4,AIO2                                                          
         MVC   ELCODE,BYTE4        R2=A(EXISTING PATTERN DATA ELEMENT)          
         BRAS  RE,GETEL                                                         
         JNE   SCGE10                                                           
         LR    R2,R4                                                            
                                                                                
SCGE10   L     R4,AIO3                                                          
         BRAS  RE,GETEL            R4=A(UPDATED PATTERN DATA ELEMENT)           
         XIT1  REGS=(R2,R4)                                                     
                                                                                
***********************************************************************         
*        ROUTINE HANDLES DELETION OF PATTERN RECORD                   *         
*        ON ENTRY ... R3 = A(IOKEY)                                   *         
***********************************************************************         
                                                                                
DELPAT   NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,VPATDR           VALIDATE PATTERN RECORD FOR                  
         JNE   NO                  DELETE/RESTORE                               
                                                                                
         USING NPTDATA,R4                                                       
         L     R4,AIO3                                                          
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         MVI   ERRFLD,D#PAREF                                                   
         TM    NPTSTAT,NPTS_DEL    ENSURE RECORD IS NOT ALREADY                 
         JO    ERR00002            DELETED                                      
         OI    NPTSTAT,NPTS_DEL    MARK RECORD DELETED                          
         CLI   RQPAUAP,C'O'                                                     
         JNE   DPAT10                                                           
         OI    NPTSTAT,NPTOPCHG                                                 
         DROP  R4                                                               
                                                                                
DPAT10   GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOXSPFIL+IO3'                        
                                                                                
         L     R4,AIO3             SET NEW CHECKSUM                             
         GOTOR (#SETCKS,ASETCKS),DMCB,AIO3,L'NPTXKEY(R4),RQPACKS                
         J     YES                                                              
                                                                                
***********************************************************************         
*        ROUTINE HANDLES RESTORE OF PATTERN RECORD                    *         
*        ON ENTRY ... R3 = A(IOKEY)                                   *         
***********************************************************************         
                                                                                
RESPAT   NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,VPATDR           VALIDATE PATTERN RECORD FOR                  
         JNE   NO                  DELETE/RESTORE                               
                                                                                
         USING NPTDATA,R4                                                       
         L     R4,AIO3                                                          
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         MVI   ERRFLD,D#PAREF                                                   
         TM    NPTSTAT,NPTS_DEL    ENSURE RECORD IS DELETED                     
         JZ    ERR00002                                                         
                                                                                
         MVC   RQPAPST,NPTSTART    SAVE ALL DATE AND TIME FIELDS                
         MVC   RQPAPEN,NPTEND                                                   
         OC    NPTSTIM,NPTSTIM                                                  
         JZ    RPAT10                                                           
         GOTOR ADJTIM,DMCB,NPTSTIM,RQPASTMA                                     
RPAT10   OC    NPTETIM,NPTETIM                                                  
         JZ    RPAT20                                                           
         GOTOR ADJTIM,DMCB,NPTETIM,RQPAETMA                                     
                                                                                
RPAT20   TM    NPTSTAT,NPTS_ADID   SET AD-ID STATUS                             
         JZ    *+8                                                              
         OI    PROSTAT,PSADID                                                   
         DROP  R4                                                               
                                                                                
         USING NPTXKEY,R3                                                       
         L     R4,AIO3             IF PATTERN DOES NOT HAVE A                   
         CLI   NPTXNET,C'$'        NETWORK LIST, ENSURE NETWORK                 
         JE    RPAT30              DOES NOT OVERLAP                             
         MVI   ERRFLD,D#PANET                                                   
         GOTOR CHKOVLP,DMCB,SVTXKEY+NPTXNET-NPTXKEY                             
         JE    RPAT50                                                           
         J     NO                                                               
         DROP  R3                                                               
                                                                                
RPAT30   MVI   ERRFLD,D#PANETS                                                  
         L     R4,AIO3                                                          
         MVI   ELCODE,X'5B'        IF PATTERN DOES HAVE A NETWORK               
         BRAS  RE,GETEL            LIST, ENSURE EACH NETWORK DOES               
         J     *+8                 NOT OVERLAP                                  
RPAT40   BRAS  RE,NEXTEL                                                        
         JNE   RPAT50                                                           
         TM    6(R4),X'80'                                                      
         JO    RPAT40                                                           
         GOTOR CHKOVLP,DMCB,2(R4)                                               
         JNE   NO                                                               
         MVI   ELCODE,X'5B'                                                     
         J     RPAT40                                                           
                                                                                
         USING NPTCMLEL,R4                                                      
RPAT50   L     R4,AIO3                                                          
         MVI   ELCODE,X'30'        GET COMMERCIAL LIST ELEMENT                  
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         LLC   R0,NPTCMLLN                                                      
         SRL   R0,4                R0=# OF COMMERCIALS                          
         LA    R4,NPTCML           R4=A(COMMERCIALS)                            
         DROP  R4                                                               
                                                                                
         USING COMTABD,R1                                                       
         L     R1,ACOMTAB                                                       
RPAT60   MVC   CTCOM,0(R4)         ADD COMMERCIAL ID                            
         MVC   CTPIG,8(R4)         AND PIGGYBACK ID TO COMMERCIAL LIST          
         LA    R1,CTLNQ(R1)                                                     
         LA    R4,16(R4)                                                        
         JCT   R0,RPAT60                                                        
         MVI   0(R1),X'FF'                                                      
         DROP  R1                                                               
                                                                                
         BAS   RE,VALCOMS          VALIDATE COMMERCIAL(S)                       
         JNE   NO                                                               
                                                                                
         MVI   DATADISP+1,42                                                    
                                                                                
         USING NPTDATA,R4                                                       
         L     R4,AIO3                                                          
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL            MARK RECORD UNDELETED                        
         JE    *+6                                                              
         DC    H'00'                                                            
         NI    NPTSTAT,X'FF'-NPTS_DEL                                           
         CLI   RQPAUAP,C'O'                                                     
         JNE   RPAT70                                                           
         OI    NPTSTAT,NPTOPCHG                                                 
         DROP  R4                                                               
                                                                                
RPAT70   GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOXSPFIL+IO3'                        
                                                                                
         L     R4,AIO3             SET NEW CHECKSUM                             
         GOTOR (#SETCKS,ASETCKS),DMCB,AIO3,L'NPTXKEY(R4),RQPACKS                
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
*        ROUTINE BUILDS COMMERCIAL AND HIATUS TABLES                  *         
***********************************************************************         
                                                                                
BLDTABS  NTR1  BASE=*,LABEL=*                                                   
         BAS   RE,BLDCOMS                                                       
         BAS   RE,BLDHIAS                                                       
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE BUILDS COMMERCIAL TABLE                              *         
***********************************************************************         
                                                                                
BLDCOMS  NTR1                                                                   
         OI    PROSTAT,PSADID      SET TO VALIDATE AS AD-ID                     
                                                                                
         USING COMTABD,R2                                                       
         L     R2,ACOMTAB          R2=A(COMMERCIAL TABLE)                       
         MVI   0(R2),C'A'          INITIALIZE ALL LETTERS/POSITIONS             
         MVI   CTLNQ(R2),C'B'      IN COMMERCIAL TABLE                          
         MVI   2*CTLNQ(R2),C'C'                                                 
         MVI   3*CTLNQ(R2),C'D'                                                 
         MVI   4*CTLNQ(R2),C'E'                                                 
         MVI   5*CTLNQ(R2),C'F'                                                 
         MVI   6*CTLNQ(R2),C'G'                                                 
         MVI   7*CTLNQ(R2),C'H'                                                 
         MVI   8*CTLNQ(R2),C'I'                                                 
         MVI   9*CTLNQ(R2),C'J'                                                 
         MVI   10*CTLNQ(R2),C'K'                                                
         MVI   11*CTLNQ(R2),C'L'                                                
                                                                                
         USING COMTABD,R2                                                       
         ZICM  R0,COMIND,1         R0=# OF COMMERCIALS                          
         JZ    BCOMS40                                                          
         CHI   R0,12                                                            
         JH    BCOMS40                                                          
         XR    R6,R6                                                            
         ZICM  R3,COMS,3           R3=A(PROVIDED COMMERCIALS)                   
BCOMS10  MVC   CTCOM,0(R3)                                                      
         OC    CTCOM,CTCOM                                                      
         JZ    BCOMS30                                                          
         CLC   CTCOM,=CL8'*'                                                    
         JE    BCOMS30                                                          
         AHI   R6,1                                                             
         GOTO1 VTRPACK,DMCB,(C'P',0(R3)),WORK                                   
         JE    BCOMS20                                                          
         NI    PROSTAT,X'FF'-PSADID                                             
         J     BCOMS30                                                          
BCOMS20  MVC   CTCOM,WORK                                                       
BCOMS30  LA    R2,CTLNQ(R2)                                                     
         LA    R3,12(R3)                                                        
         JCT   R0,BCOMS10                                                       
         STC   R6,COMINDP                                                       
                                                                                
BCOMS40  ZICM  R0,PIGIND,1         R0=# OF PIGGYBACK COMMERCIALS                
         JZ    BCOMS80                                                          
         CHI   R0,12                                                            
         JH    BCOMS80                                                          
         XR    R6,R6                                                            
         L     R2,ACOMTAB          R2=A(COMMERCIAL TABLE)                       
         ZICM  R3,PIGS,3           R3=A(PROVIDED PIGGYBACK COMMERCIALS)         
BCOMS50  MVC   CTPIG,0(R3)                                                      
         OC    CTPIG,CTPIG                                                      
         JZ    BCOMS70                                                          
         AHI   R6,1                                                             
         GOTO1 VTRPACK,DMCB,(C'P',0(R3)),WORK                                   
         JE    BCOMS60                                                          
         NI    PROSTAT,X'FF'-PSADID                                             
         J     BCOMS70                                                          
BCOMS60  MVC   CTPIG,WORK                                                       
BCOMS70  LA    R2,CTLNQ(R2)                                                     
         LA    R3,12(R3)                                                        
         JCT   R0,BCOMS50                                                       
         STC   R6,PIGINDP                                                       
                                                                                
BCOMS80  ZICM  R0,PCTIND,1         R0=# OF PERCENTAGES                          
         JZ    BCOMS120                                                         
         CHI   R0,12                                                            
         JH    BCOMS120                                                         
         XR    R6,R6                                                            
         L     R2,ACOMTAB          R2=A(COMMERCIAL TABLE)                       
         ZICM  R3,PCTS,3           R3=A(PROVIDED PERCENTAGES)                   
BCOMS90  MVC   CTPCT,0(R3)                                                      
         MVC   CTOPCT,0(R3)                                                     
         OC    CTPCT,CTPCT                                                      
         JZ    BCOMS100                                                         
         AHI   R6,1                                                             
BCOMS100 LA    R2,CTLNQ(R2)                                                     
         LA    R3,3(R3)                                                         
         JCT   R0,BCOMS90                                                       
         STC   R6,PCTINDP                                                       
         DROP  R2                                                               
                                                                                
BCOMS120 ZIC   RE,COMIND           MARK END OF TABLE                            
         ZIC   RF,PIGIND                                                        
         CR    RE,RF                                                            
         JNL   BCOMS130                                                         
         CHI   RF,12                                                            
         JH    BCOMS130                                                         
         LR    RE,RF                                                            
BCOMS130 ZIC   RF,PCTIND                                                        
         CR    RE,RF                                                            
         JNL   BCOMS140                                                         
         CHI   RF,12                                                            
         JH    BCOMS140                                                         
         LR    RE,RF                                                            
BCOMS140 MHI   RE,CTLNQ                                                         
         L     RF,ACOMTAB                                                       
         AR    RF,RE                                                            
         MVI   0(RF),X'FF'                                                      
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE BUILDS HIATUS TABLE                                  *         
***********************************************************************         
                                                                                
BLDHIAS  NTR1                                                                   
         USING HIATABD,R2                                                       
         ZICM  R0,HDYIND,1         R0=# OF HIATUS DAYS                          
         JZ    BHIAS20                                                          
         CHI   R0,12                                                            
         JH    BHIAS20                                                          
         XR    R1,R1                                                            
         L     R2,AHIATAB                                                       
         ZICM  R3,HDYS,3           R3=A(HIATUS DAYS)                            
BHIAS10  MVC   HTDAY,0(R3)                                                      
         OC    HTDAY,HTDAY                                                      
         JZ    *+8                                                              
         AHI   R1,1                                                             
         LA    R2,HTLNQ(R2)                                                     
         LA    R3,10(R3)                                                        
         JCT   R0,BHIAS10                                                       
         STC   R1,HDYINDP                                                       
                                                                                
BHIAS20  ZICM  R0,HDTIND,1         R0=# OF HIATUS DATES                         
         JZ    BHIAS40                                                          
         CHI   R0,12                                                            
         JH    BHIAS40                                                          
         XR    R1,R1                                                            
         L     R2,AHIATAB                                                       
         ZICM  R3,HDTS,3           R3=A(HIATUS DATES)                           
BHIAS30  MVC   HTDAT,0(R3)                                                      
         OC    HTDAT,HTDAT                                                      
         JZ    *+8                                                              
         AHI   R1,1                                                             
         LA    R2,HTLNQ(R2)                                                     
         LA    R3,3(R3)                                                         
         JCT   R0,BHIAS30                                                       
         STC   R1,HDTINDP                                                       
         LTR   R1,R1                                                            
         JZ    BHIAS40                                                          
         L     R2,AHIATAB                                                       
         XC    HTDAY,HTDAY                                                      
                                                                                
BHIAS40  ZICM  R0,HSTIND,1         R0=# OF HIATUS START TIMES                   
         JZ    BHIAS60                                                          
         CHI   R0,12                                                            
         JH    BHIAS60                                                          
         XR    R1,R1                                                            
         L     R2,AHIATAB                                                       
         ZICM  R3,HSTS,3           R3=A(HIATUS START TIMES)                     
BHIAS50  MVC   HTSTM,0(R3)                                                      
         OC    HTSTM,HTSTM                                                      
         JZ    *+8                                                              
         AHI   R1,1                                                             
         LA    R2,HTLNQ(R2)                                                     
         LA    R3,5(R3)                                                         
         JCT   R0,BHIAS50                                                       
         STC   R1,HSTINDP                                                       
                                                                                
BHIAS60  ZICM  R0,HETIND,1         R0=# OF HIATUS END TIMES                     
         JZ    BHIAS80                                                          
         CHI   R0,12                                                            
         JH    BHIAS80                                                          
         XR    R1,R1                                                            
         L     R2,AHIATAB                                                       
         ZICM  R3,HETS,3           R3=A(HIATUS END TIMES)                       
BHIAS70  MVC   HTETM,0(R3)                                                      
         OC    HTETM,HTETM                                                      
         JZ    *+8                                                              
         AHI   R1,1                                                             
         LA    R2,HTLNQ(R2)                                                     
         LA    R3,5(R3)                                                         
         JCT   R0,BHIAS70                                                       
         STC   R1,HETINDP                                                       
         DROP  R2                                                               
                                                                                
BHIAS80  ZIC   RE,HDYIND           MARK END OF TABLE                            
         ZIC   RF,HDTIND                                                        
         CR    RE,RF                                                            
         JNL   BHIAS90                                                          
         CHI   RF,12                                                            
         JH    BHIAS90                                                          
         LR    RE,RF                                                            
BHIAS90  ZIC   RF,HSTIND                                                        
         CR    RE,RF                                                            
         JNL   BHIAS100                                                         
         CHI   RF,12                                                            
         JH    BHIAS90                                                          
         LR    RE,RF                                                            
BHIAS100 ZIC   RF,HETIND                                                        
         CR    RE,RF                                                            
         JNL   BHIAS110                                                         
         CHI   RF,12                                                            
         JH    BHIAS110                                                         
         LR    RE,RF                                                            
BHIAS110 MHI   RE,HTLNQ                                                         
         L     RF,AHIATAB                                                       
         AR    RF,RE                                                            
         MVI   0(RF),X'FF'                                                      
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE ASSERTS ALL REQUIRED/NOT ALLOWED RULES ARE SATISIFED *         
*        FOR DELETES AND RESTORES                                     *         
***********************************************************************         
                                                                                
AREQDR   NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,AREQALL          ASSERT REQUIRED/NOT ALLOWED RULES            
         JNE   NO                  FOR ADD/CHANGE/DELETE/RESTORE                
                                                                                
         MVI   ERRFLD,D#PAREF                                                   
         OC    RQPAREF,RQPAREF     ASSERT THAT REFERENCE NUMBER IS              
         JZ    ERR00001            PROVIDED                                     
                                                                                
         MVI   ERRFLD,D#PADSC                                                   
         OC    RQPADSC,RQPADSC     ASSERT THAT DESCRIPTION IS NOT               
         JNZ   ERR00006            NOT PROVIDED                                 
                                                                                
         MVI   ERRFLD,D#PAPST                                                   
         OC    RQPAPST,RQPAPST     ASSERT THAT PERIOD START DATE                
         JNZ   ERR00006            IS NOT PROVIDED                              
                                                                                
         MVI   ERRFLD,D#PAPEN                                                   
         OC    RQPAPEN,RQPAPEN     ASSERT THAT PERIOD END DATE                  
         JNZ   ERR00006            IS NOT PROVIDED                              
                                                                                
         MVI   ERRFLD,D#PAUFN                                                   
         CLI   RQPAUFN,0           ASSERT THAT UFN DATE? IS NOT                 
         JNE   ERR00006            PROVIDED                                     
                                                                                
         MVI   ERRFLD,D#PASTM                                                   
         OC    RQPASTM,RQPASTM     ASSERT THAT START TIME IS NOT                
         JNZ   ERR00006            PROVIDED                                     
                                                                                
         MVI   ERRFLD,D#PAETM                                                   
         OC    RQPAETM,RQPAETM     ASSERT THAT END TIME IS NOT                  
         JNZ   ERR00006            PROVIDED                                     
                                                                                
         MVI   ERRFLD,D#PACKS                                                   
         OC    RQPACKS,RQPACKS     ASSERT THAT CHECK SUM IS NOT                 
         JNZ   ERR00006            PROVIDED                                     
                                                                                
         MVI   ERRFLD,D#PACOMS                                                  
         CLI   COMIND,0            ASSERT THAT COMMERCIALS ARE NOT              
         JNE   ERR00006            PROVIDED                                     
                                                                                
         MVI   ERRFLD,D#PAPIGS                                                  
         CLI   PIGIND,0            ASSERT THAT PIGGYBACK COMMERCIALS            
         JNE   ERR00006            ARE NOT PROVIDED                             
                                                                                
         MVI   ERRFLD,D#PAPCTS                                                  
         CLI   PCTIND,0            ASSERT THAT PERCENTAGES ARE NOT              
         JNE   ERR00006            PROVIDED                                     
                                                                                
         MVI   ERRFLD,D#PAROT                                                   
         CLI   ROTIND,0            ASSERT THAT ROTATION IS NOT                  
         JNE   ERR00006            PROVIDED                                     
                                                                                
         MVI   ERRFLD,D#PACMTS                                                  
         CLI   CMTIND,0            ASSERT THAT COMMENTS ARE NOT                 
         JNE   ERR00006            PROVIDED                                     
                                                                                
         MVI   ERRFLD,D#PAHDYS                                                  
         CLI   HDYIND,0            ASSERT THAT HIATUS DAYS ARE NOT              
         JNE   ERR00006            PROVIDED                                     
                                                                                
         MVI   ERRFLD,D#PAHDTS                                                  
         CLI   HDTIND,0            ASSERT THAT HIATUS DATES ARE NOT             
         JNE   ERR00006            PROVIDED                                     
                                                                                
         MVI   ERRFLD,D#PAHSTS                                                  
         CLI   HSTIND,0            ASSERT THAT HIATUS START TIMES ARE           
         JNE   ERR00006            NOT PROVIDED                                 
                                                                                
         MVI   ERRFLD,D#PAHETS                                                  
         CLI   HETIND,0            ASSERT THAT HIATUS END TIMES ARE             
         JNE   ERR00006            NOT PROVIDED                                 
         J     YES                                                              
                                                                                
***********************************************************************         
*        ROUTINE ASSERTS ALL REQUIRED/NOT ALLOWED RULES ARE SATISIFED *         
*        FOR ADDS, CHANGES, DELETES AND RESTORES                      *         
***********************************************************************         
                                                                                
AREQALL NTR1   BASE=*,LABEL=*                                                   
         MVI   ERRFLD,D#PACLT                                                   
         OC    RQPACLT,RQPACLT     ASSERT THAT CLIENT IS PROVIDED               
         JZ    ERR00001                                                         
                                                                                
         XR    R1,R1                                                            
         CLI   RQPANMD,0           ASSERT THAT NO MORE THAN ONE                 
         JE    *+8                 OF NETWORK MEDIA                             
         AHI   R1,1                                                             
         CLI   RQPANET,0           NETWORK                                      
         JE    *+12                                                             
         AHI   R1,1                                                             
         MVI   ERRFLD,D#PANET                                                   
         CLI   NETIND,0            OR NETWORK LIST IS PROVIDED                  
         JE    *+12                                                             
         AHI   R1,1                                                             
         MVI   ERRFLD,D#PANETS                                                  
         CHI   R1,1                                                             
         JH    ERR00006                                                         
                                                                                
         MVI   ERRFLD,D#PAPRD                                                   
         OC    RQPAPRD,RQPAPRD     ASSERT THAT PRODUCT IS PROVIDED              
         JZ    ERR00001                                                         
                                                                                
         CLI   RQPAUAP,0           ASSERT THAT UPDATING APPLICATION             
         JNE   YES                 IS PROVIDED                                  
         MVI   ERRFLD,D#PAUAP                                                   
         J     ERR00001                                                         
         EJECT                                                                  
***********************************************************************         
*        ROUTINE VALIDATES PATTERN RECORD FOR ADD/CHANGE              *         
*        ON ENTRY ... R3 = A(IOKEY)                                   *         
***********************************************************************         
                                                                                
VPATAC   NTR1  BASE=*,LABEL=*                                                   
         BAS   RE,AREQAC           ASSERT REQUIRED/NOT ALLOWED                  
         JNE   NO                  RULES FOR ADD/CHANGE ARE SATISIFIED          
                                                                                
         BRAS  RE,VPATALL          VALIDATE PATTERN RECORD FOR                  
         JNE   NO                  ALL ACTIONS                                  
                                                                                
         BAS   RE,VALUFN           VALIDATE UFN DATE                            
         JNE   NO                                                               
                                                                                
         BAS   RE,VALCOMS          VALIDATE COMMERCIAL(S)                       
         JNE   NO                                                               
                                                                                
         BRAS  RE,VALROT           VALIDATE ROTATION                            
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE ASSERTS REQUIRED/NOT ALLOWED RULES ARE SATSIFIED     *         
*        FOR ADDS AND CHANGES                                         *         
***********************************************************************         
                                                                                
AREQAC   NTR1                                                                   
         BRAS  RE,AREQALL          ASSERT REQUIRED/NOT ALLOWED RULES            
         JNE   NO                  ARE SATISIFIED FOR ALL ACTIONS               
                                                                                
         OC    RQPANET,RQPANET     IF NETWORK IS NOT PROVIDED                   
         JNZ   ARAC10                                                           
         MVI   ERRFLD,D#PAPRO                                                   
         OC    RQPAPRO,RQPAPRO     ASSERT THAT PROGRAM                          
         JNZ   ERR00624                                                         
         MVI   ERRFLD,D#PAFED                                                   
         OC    RQPAFED,RQPAFED     AND FEED ARE NOT PROVIDED                    
         JNZ   ERR00624                                                         
                                                                                
ARAC10   OC    RQPAPRO,RQPAPRO     IF PROGRAM IS PROVIDED                       
         JZ    ARAC20                                                           
         MVI   ERRFLD,D#PADPT                                                   
         OC    RQPADPT,RQPADPT     ASSERT THAT DAYPART CODE IS NOT              
         JNZ   ERR00626            PROVIDED                                     
         MVI   ERRFLD,D#PAFED                                                   
         OC    RQPAFED,RQPAFED     ASSERT THAT FEED IS NOT PROVIDED             
         JNZ   ERR00629                                                         
                                                                                
ARAC20   OC    RQPADPT,RQPADPT     IF DAYPART IS PROVIDED                       
         JZ    ARAC30                                                           
         CLI   RQPANMD,0           ASSERT THAT NETWORK MEDIA                    
         JNE   ARAC30                                                           
         OC    RQPANET,RQPANET     NETWORK                                      
         JNZ   ARAC30                                                           
         CLI   NETIND,0            OR NETWORKS ARE PROVIDED                     
         JNE   ARAC30                                                           
         MVI   ERRFLD,D#PADPT                                                   
         J     ERR00624                                                         
                                                                                
ARAC30   CLI   RQPAPPL,0           IF PRODUCT PARTNER LENGTH IS                 
         JE    ARAC40              PROVIDED                                     
         MVI   ERRFLD,D#PAPPR                                                   
         OC    RQPAPPR,RQPAPPR     ASSERT THAT PRODUCT PARTNER                  
         JZ    ERR00001            IS PROVIDED                                  
                                                                                
ARAC40   MVI   ERRFLD,D#PADSC                                                   
         OC    RQPADSC,RQPADSC     ASSERT THAT DESCRIPTION IS                   
         JZ    ERR00001            PROVIDED                                     
                                                                                
         MVI   ERRFLD,D#PAPST                                                   
         OC    RQPAPST,RQPAPST     ASSERT THAT PERIOD START DATE                
         JZ    ERR00001            IS PROVIDED                                  
                                                                                
         MVI   ERRFLD,D#PAUFN                                                   
         OC    RQPAPEN,RQPAPEN     IF PERIOD END DATE IS PROVIDED               
         JZ    ARAC50                                                           
         CLI   RQPAUFN,0           ASSERT THAT UFN DATE? IS NOT                 
         JNE   ERR00006            PROVIDED                                     
         J     ARAC60                                                           
                                                                                
ARAC50   CLI   RQPAUFN,0           IF PERIOD END DATE IS NOT PROVIDED           
         JE    ERR00001            ASSERT THAT UFN DATE? IS PROVIDED            
                                                                                
ARAC60   MVI   ERRFLD,D#PASTM                                                   
         OC    RQPAPRO,RQPAPRO     IF PROGRAM                                   
         JNZ   ARAC70                                                           
         OC    RQPADPT,RQPADPT     OR DAYPART IS PROVIDED                       
         JZ    ARAC80                                                           
ARAC70   OC    RQPASTM,RQPASTM     ASSERT THAT PERIOD START DATE                
         JNZ   ERR00781            IS NOT PROVIDED                              
                                                                                
ARAC80   OC    RQPAETM,RQPAETM     IF PERIOD END TIME IS PROVIDED               
         JZ    ARAC90                                                           
         MVI   ERRFLD,D#PASTM                                                   
         OC    RQPASTM,RQPASTM     ASSERT THAT PERIOD START TIME                
         JZ    ERR00001            IS PROVIDED                                  
         CLI   RQPAUFN,0           ASSERT THAT UFN DATE? IS NOT                 
         JE    ARAC100             PROVIDED                                     
         MVI   ERRFLD,D#PAETM                                                   
         JNE   ERR00006                                                         
                                                                                
ARAC90   MVI   ERRFLD,D#PAETM      IF PERIOD END TIME IS NOT PROVIDED           
         OC    RQPASTM,RQPASTM     AND PERIOD START TIME IS                     
         JZ    ARAC100                                                          
         CLI   RQPAUFN,0           ENSURE THAN UFN DATE? IS PROVIDED            
         JE    ERR00001                                                         
                                                                                
ARAC100  MVI   ERRFLD,D#PACOMS                                                  
         CLI   COMINDP,0           ASSERT AT LEAST 1 COMMERCIAL IS              
         JE    ERR00001            PROVIDED                                     
                                                                                
         ZIC   RE,COMIND                                                        
         ZICM  RF,COMS,3           ASSERT AT LEAST 1 "REAL"                     
ARAC110  CLC   =CL12'*',0(RF)      COMMERCIAL IS PROVIDED                       
         JE    ARAC120                                                          
         OC    0(12,RF),0(RF)                                                   
         JNZ   ARAC130                                                          
ARAC120  LA    RF,12(RF)                                                        
         JCT   RE,ARAC110                                                       
         J     ERR00001                                                         
                                                                                
ARAC130  CLI   PIGINDP,0           IF ANY PIGGYBACK COMMERCIALS ARE             
         JE    ARAC150             PROVIDED                                     
         OC    RQPAPPR,RQPAPPR     ENSURE PRODUCT PARTNER IS PROVIDED           
         JNZ   ARAC150                                                          
         MVI   ERRFLD,D#PAPPR                                                   
         ZIC   RE,PIGIND                                                        
         ZICM  RF,PIGS,3                                                        
ARAC140  OC    0(12,RF),0(RF)                                                   
         JNZ   ERR00006                                                         
         LA    RF,12(RF)                                                        
         JCT   RE,ARAC140                                                       
                                                                                
ARAC150  MVI   ERRFLD,D#PAROT                                                   
         CLI   PCTINDP,0           IF NO PERCENTAGES ARE PROVIDED               
         JNE   ARAC160                                                          
         CLI   ROTIND,0            ENSURE ROTATION IS PROVIDED                  
         JE    ERR00768                                                         
         J     ARAC170                                                          
                                                                                
ARAC160  CLI   ROTIND,0            IF PERCENTAGES ARE PROVIDED                  
         JNE   ERR00006            ENSURE ROTATION IS NOT PROVIDED              
                                                                                
ARAC170  MVI   ERRFLD,D#PAHDYS                                                  
         CLI   HDYIND,12           ENSURE THE NUMBER OF HIATUS DAYS             
         JH    ERR00002            DOES NOT EXCEED 12                           
                                                                                
         MVI   ERRFLD,D#PAHDTS                                                  
         CLI   HDTIND,12           ENSURE THE NUMBER OF HIATUS DATES            
         JH    ERR00002            DOES NOT EXCEED 12                           
                                                                                
         MVI   ERRFLD,D#PAHSTS                                                  
         CLI   HSTIND,12           ENSURE THE NUMBER OF HIATUS START            
         JH    ERR00002            TIMES DOES NOT EXCEED 12                     
                                                                                
         MVI   ERRFLD,D#PAHETS                                                  
         CLI   HETIND,12           ENSURE THE NUMBER OF HIATUS END              
         JH    ERR00002            TIMES DOES NOT EXCEED 12                     
                                                                                
         USING HIATABD,R2                                                       
         L     R2,AHIATAB                                                       
ARAC180  CLI   0(R2),X'FF'         IF ANY HIATUSES ARE PROVIDED ...             
         JE    YES                                                              
                                                                                
         MVI   ERRFLD,D#PAHDYS                                                  
         CLI   HDYINDP,0           ENSURE ONLY DAYS OR DATES ARE                
         JE    ARAC200             PROVIDED (CAN'T HAVE MIX OF DAYS             
         CLI   HDTINDP,0           AND DATES)                                   
         JE    ARAC190                                                          
         MVI   ERRFLD,D#PAHDTS                                                  
         J     ERR00810                                                         
                                                                                
ARAC190  ZIC   RE,HDYINDP          IF DAYS ARE PROVIDED, ENSURE                 
         SHI   RE,1                ALL HIATUSES EXCEPT ONE HAVE                 
         ZIC   RF,HSTINDP          HIATUS START TIME                            
         CR    RE,RF                                                            
         JH    ERR00804                                                         
                                                                                
ARAC200  OC    HTDAY,HTDAY         FOR EACH ENTRY IN HIATUS TABLE ...           
         JNZ   ARAC210                                                          
         OC    HTDAT,HTDAT         ENSURE THAT DAY OR DATE IS                   
         JZ    ERR00001            PROVIDED                                     
                                                                                
ARAC210  XR    R0,R0                                                            
         OC    HTSTM,HTSTM         ENSURE NEITHER OR BOTH TIMES                 
         JZ    *+12                ARE PROVIDED (NOT JUST ONE)                  
         MVI   ERRFLD,D#PAHETS                                                  
         AHI   R0,1                                                             
         OC    HTETM,HTETM                                                      
         JZ    *+12                                                             
         MVI   ERRFLD,D#PAHSTS                                                  
         AHI   R0,1                                                             
         CHI   R0,1                                                             
         JE    ERR00001                                                         
                                                                                
         LA    R2,HTLNQ(R2)                                                     
         J     ARAC180                                                          
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        ROUTINE VALIDATES UFN DATE                                   *         
***********************************************************************         
                                                                                
VALUFN   NTR1                                                                   
         CLI   RQPAUFN,C'Y'        IF UFN DATE IS PROVIDED                      
         JNE   YES                                                              
         CLI   SVTN1PRO,C'Y'       ENSURE TN1 PROFILE ALLOWS THEM               
         JE    ERR00632                                                         
         MVC   RQPAPEN,=3X'FF'     AND SET PERIOD END DATE                      
         J     YES                                                              
                                                                                
***********************************************************************         
*        ROUTINE PROCESSES COMMERCIAL(S)                              *         
***********************************************************************         
                                                                                
VALCOMS  NTR1  BASE=*,LABEL=*                                                   
         USING COMTABD,R2                                                       
         L     R2,ACOMTAB          R2=A(COMMERCIAL TABLE)                       
                                                                                
VCOMS10  MVI   COMSTAT,0                                                        
         BAS   RE,VALCOM1          VALIDATE COMMERCIAL                          
         JNE   NO                                                               
         BAS   RE,VALCOM2          PIGGYBACK COMMERCIAL                         
         JNE   NO                                                               
         BAS   RE,VALPCT           AND PERCENTAGE                               
         JNE   NO                                                               
                                                                                
VCOMS20  CLI   CTLNQ(R2),X'FF'                                                  
         JE    YES                                                              
         LA    R2,CTLNQ(R2)        BUMP TO NEXT COMMERCIAL                      
         J     VCOMS10                                                          
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        ROUTINE PROCESSES COMMERCIAL 1                               *         
*        ON ENTRY ... R2 = A(COMMERCIAL TABLE ENTRY)                  *         
*                     R3 = A(IOKEY)                                   *         
***********************************************************************         
                                                                                
         USING COMTABD,R2                                                       
VALCOM1  NTR1                                                                   
         CLC   CTCOM,=CL8'*'       IF COMMERCIAL 1 IS NOT BEING                 
         JE    YES                 DELETED ...                                  
                                                                                
         MVI   ERRFLD,D#PACOMS                                                  
         GOTO1 VCOM,DMCB,CTCOM     VALIDATE COMMERCIAL 1                        
         JNE   NO                                                               
                                                                                
         USING CMLDTAEL,R4                                                      
         L     R4,AIO4                                                          
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL            R4=A(COMMERCIAL DATA ELEMENT)                
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   CTCOMTYP,CMLTYPE    SAVE COMMERCIAL TYPE                         
                                                                                
***********************************************************************         
                                                                                
         OC    RQPAPPR,RQPAPPR     IF PARTNER PRODUCT IS NOT PROVIDED           
         JNZ   VC110                                                            
         CLI   CMLSOLO,C'P'        ENSURE COMMERCIAL IS NOT PIGGYBACK           
         JE    ERR00696            ONLY                                         
         GOTO1 CHKPRD,DMCB,RQPAPRD ENSURE PRODUCT IS ON COMMERCIAL              
         JNE   NO                                                               
         CLC   RQPAPRL,CMLSLN      ENSURE PRODUCT LENGTH MATCHES COM'L          
         JNE   ERR00076                                                         
         J     YES                                                              
                                                                                
***********************************************************************         
                                                                                
VC110    CLI   CMLSOLO,C'S'        IF PARTNER PRODUCT IS PROVIDED               
         JE    ERR00696            ENSURE COMMERCIAL IS NOT SOLO ONLY           
                                                                                
***********************************************************************         
                                                                                
         OC    CTPIG,CTPIG         IF PIGGYBACK COMMERCIAL IS NOT               
         JNZ   VC120               PROVIDED                                     
         GOTO1 CHKPRD,DMCB,RQPAPRD ENSURE PRODUCT 1 IS ON COMMERCIAL            
         JNE   NO                                                               
         LLC   R0,CMLSLN           AND PRODUCT 1 LENGTH MATCHES                 
         SRL   R0,1                COMMERCIAL LENGTH DIVIDED BY 2               
         CLM   R0,1,RQPAPRL                                                     
         JNE   ERR00076                                                         
         GOTO1 CHKPRD,DMCB,RQPAPPR AND PRODUCT PARTNER IS ON                    
         JNE   NO                  COMMERCIAL                                   
         CLM   R0,1,RQPAPPL        AND PRODUCT PARTNER LENGTH MATCHES           
         JNE   ERR00076            COMMERCIAL LENGTH DIVIDED BY 2               
         J     YES                                                              
                                                                                
***********************************************************************         
                                                                                
VC120    GOTO1 CHKPRD,DMCB,RQPAPRD IF PIGGYBACK COMMERCIAL IS PROVIDED          
         JNE   VC130               ENSURE PRODUCT 1 IS ON COMMERCIAL            
         CLC   RQPAPRL,CMLSLN      AND MATCHES LENGTH                           
         JNE   VC130                                                            
         OI    COMSTAT,CSP1MTCH                                                 
         J     YES                                                              
                                                                                
VC130    GOTO1 CHKPRD,DMCB,RQPAPPR OR THAT PRODUCT 2 IS ON COMMERCIAL           
         JNE   NO                  AND MATCHES LENGTH                           
         CLC   RQPAPPL,CMLSLN                                                   
         JNE   ERR00076                                                         
         OI    COMSTAT,CSP2MTCH                                                 
         J     YES                                                              
         DROP  R2,R4                                                            
                                                                                
***********************************************************************         
*        ROUTINE PROCESSES COMMERCIAL 2                               *         
*        ON ENTRY ... R2 = A(COMMERCIAL TABLE ENTRY)                  *         
*                     R3 = A(IOKEY)                                   *         
***********************************************************************         
                                                                                
         USING COMTABD,R2                                                       
VALCOM2  NTR1                                                                   
         OC    CTPIG,CTPIG         IF PIGGYBACK COMMERCIAL IS                   
         JZ    YES                 PROVIDED                                     
                                                                                
         CLC   CTCOM,=CL8'*'        ENSURE REAL COMMERCIAL IS PROVIDED          
         JE    ERR00002                                                         
         OC    CTCOM,CTCOM                                                      
         JZ    ERR00001                                                         
                                                                                
         MVI   ERRFLD,D#PAPIGS                                                  
         CLC   CTCOM,CTPIG         ENSURE PIGGYBACK COMMERCIAL DOES             
         JE    ERR00621            NOT MATCH COMMERCIAL                         
                                                                                
         GOTO1 VCOM,DMCB,CTPIG     VALIDATE PIGGYBACK COMMERCIAL                
         JNE   NO                                                               
                                                                                
         USING CMLDTAEL,R4                                                      
         L     R4,AIO4                                                          
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL            R4=A(COMMERCIAL DATA ELEMENT)                
         BE    *+6                                                              
         DC    H'0'                                                             
         JNE   NO                                                               
                                                                                
         CLI   CMLSOLO,C'S'        ENSURE COMMERCIAL IS NOT FOR SOLO            
         JE    ERR00696            ONLY                                         
                                                                                
         CLC   CTCOMTYP,CMLTYPE    ENSURE BOTH COMMERCIALS ARE                  
         JNE   ERR00078            THE SAME TYPE                                
                                                                                
         TM    COMSTAT,CSP1MTCH    IF COMMERCIAL 1 MATCHED PRODUCT 1            
         JZ    VC210                                                            
         GOTO1 CHKPRD,DMCB,RQPAPPR ENSURE PRODUCT 2 IS ON COMMERCIAL            
         JNE   NO                  AND MATCHES LENGTH                           
         CLC   RQPAPPL,CMLSLN                                                   
         JNE   ERR00076                                                         
         J     YES                                                              
                                                                                
VC210    TM    COMSTAT,CSP2MTCH    IF COMMERCIAL 1 MATCHED PRODUCT 2            
         JO    *+6                                                              
         DC    H'00'                                                            
         GOTO1 CHKPRD,DMCB,RQPAPRD ENSURE PRODUCT 1 IS ON COMMERCIAL            
         JNE   NO                  AND MATCHES LENGTH                           
         CLC   RQPAPRL,CMLSLN                                                   
         JNE   ERR00076                                                         
         J     YES                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE VALIDATES COMMERCIAL                                 *         
*        ON ENTRY ... R3 = A(IOKEY)                                   *         
*                     P1 = A(COMMERCIAL FIELD TO VALIDATE)            *         
***********************************************************************         
                                                                                
VCOM     NTR1                                                                   
         L     R2,0(R1)            R2=A(COMMERCIAL TO VALIDATE)                 
                                                                                
         GOTO1 READCOM,DMCB,0(R2),=AL2(IO4)                                     
         JNE   NO                  ENSURE COMMERCIAL EXISTS                     
                                                                                
         USING CMLDTAEL,R4                                                      
         L     R4,AIO4                                                          
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL            R4=A(COMMERCIAL DATA ELEMENT)                
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    CMLSTAT,X'80'       ENSURE COMMERCIAL IS NOT DELETED             
         JO    ERR00535                                                         
         CLC   CMLRLSE,RQPAPST     ENSURE RELEASE DATE IS EQUAL OR              
         JH    ERR00077            EARLIER THAN PATTERN START DATE              
                                                                                
         CLI   RQPAUFN,C'Y'        IF NOT UFN DATE                              
         JE    VCOM10                                                           
         CLC   CMLRCL,RQPAPEN      ENSURE RECALL DATE IS EQUAL OR               
         JL    ERR00093            LATER THAN PATTERN END DATE                  
                                                                                
VCOM10   MVC   SVCMLRCL,CMLRCL     SAVE COMMERCIAL RECALL DATE                  
         DROP  R4                                                               
                                                                                
         USING CMLMATEL,R4                                                      
         OC    RQPASTM,RQPASTM     IF PATTERN HAS START TIME                    
         JZ    VCOM20                                                           
         L     R4,AIO4                                                          
         MVI   ELCODE,X'B0'                                                     
         BRAS  RE,GETEL                                                         
         BNE   VCOM20                                                           
         OC    CMLMSTIM,CMLMSTIM   ENSURE COMMERCIAL DOES NOT                   
         JNZ   ERR00777                                                         
         DROP  R4                                                               
                                                                                
         USING CMLNETEL,R4                                                      
VCOM20   L     R4,AIO4                                                          
         MVI   ELCODE,X'22'        READ COMMERCIALS NETWORK ELEMENTS            
         BRAS  RE,GETEL                                                         
         JNE   YES                                                              
                                                                                
VCOM30   CLI   CMLNETLN,6          IF THIS IS NEW STYLE ELEMENT                 
         JE    VCOM40                                                           
         TM    CMLFLG,CMLEXNET     AND STATUS SET TO EXCLUDE NETWORK            
         JZ    VCOM40                                                           
         OC    NETWORK,NETWORK     RETURN ERROR IF PATTERN IS FOR               
         JZ    ERR00785            ALL NETWORKS                                 
         CLC   NETWORK,CMLNET      OR FOR THE EXCLUDED NETWORK                  
         JE    ERR00785                                                         
         J     YES                                                              
                                                                                
VCOM40   CLC   NETWORK,CMLNET      IF OLD STYLE ELEMENT OR NOT SET              
         JE    YES                 TO EXCLUDE, OK IF NETWORK IS FOUND           
                                                                                
VCOM50   BRAS  RE,NEXTEL                                                        
         JNE   ERR00785                                                         
         J     VCOM30                                                           
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE READS COMMERCIAL RECORD                              *         
*        ON ENTRY ... R3 = A(IOKEY)                                   *         
*                     P1 = A(COMMERCIAL ID/AD-ID)                     *         
*                     P2 = I/O AREA EQUATE FOR READ                   *         
***********************************************************************         
                                                                                
READCOM  NTR1                                                                   
         L     R2,0(R1)            R2=A(COMMERCIAL ID/AD-ID)                    
         L     R4,4(R1)                                                         
         LH    R4,0(R4)            R4=I/O AREA EQUATE                           
                                                                                
         USING CMLKEY,R3                                                        
         XC    IOKEY,IOKEY                                                      
                                                                                
         TM    PROSTAT,PSADID      IF VALIDATING ADIDS                          
         JZ    RCOM10              BUILD KEY                                    
         MVC   CMLPIDAD,=X'0AC1'                                                
         MVC   CMLPADAM,QMEDX                                                   
         MVC   CMLPADCL,QCLTX                                                   
         MVC   CMLPADID,0(R2)                                                   
         J     RCOM20                                                           
                                                                                
RCOM10   MVC   CMLKID,=X'0A21'     IF VALIDATING COMMERCIAL IDS                 
         MVC   CMLKAM,QMEDX        BUILD KEY                                    
         MVC   CMLKCLT,QCLTX                                                    
         MVC   CMLKCML,0(R2)                                                    
                                                                                
RCOM20   LR    R1,R4               SET I/O AREA                                 
         AHI   R1,IOHI             SET READ HIGH                                
         AHI   R1,IOSPTDIR         SET EXTENDED SPOT DIRECTORY                  
         GOTOR (#IOEXEC,AIOEXEC)                                                
         CLC   IOKEY(13),IOKEYSAV  ENSURE COMMERCIAL EXISTS                     
         JNE   ERR00064                                                         
         DROP  R3                                                               
                                                                                
         LR    R1,R4               SET I/O AREA                                 
         AHI   R1,IOGET            SET GETREC                                   
         AHI   R1,IOSPTFIL         SET EXTENDED SPIT FILE                       
         GOTOR (#IOEXEC,AIOEXEC)   AND READ COMMERCIAL RECORD                   
                                                                                
         MVI   DATADISP+1,24                                                    
         J     YES                                                              
                                                                                
***********************************************************************         
*        ROUTINE ENSURE PRODUCT IS ON COMMERCIAL                      *         
*        ON ENTRY ... AIO4 = A(COMMERCIAL RECORD)                     *         
*                     P1   = A(PRODUCT CODE)                          *         
***********************************************************************         
                                                                                
CHKPRD   NTR1                                                                   
         L     R2,0(R1)            R2=A(PRODUCT CODE)                           
                                                                                
         USING CMLMPREL,R4                                                      
         L     R4,AIO4                                                          
         MVI   ELCODE,X'29'        READ COMMERCIAL RECORD'S                     
         BRAS  RE,GETEL            PRODUCT LIST ELEMENT                         
         JE    *+6                                                              
         DC    H'0'                                                             
         CLI   CMLMPRS,X'FF'       PRODUCT IS VALID IF COMMERCIAL               
         JE    YES                 IS FOR ALL PRODUCTS                          
                                                                                
         LLC   RF,CMLMPRLN                                                      
         AHI   RF,-2                                                            
         SR    RE,RE                                                            
         D     RE,=F'3'            OR PRODUCT IS IN PRODUCT LIST                
         LA    RE,CMLMPRS                                                       
CPRD10   CLC   0(L'RQPAPRD,R2),0(RE)                                            
         JE    YES                                                              
         LA    RE,3(RE)                                                         
         JCT   RF,CPRD10                                                        
         J     ERR00094                                                         
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE VALIDATES PERCENTAGE                                 *         
*        ON ENTRY ... R2 = A(COMMERCIAL TABLE ENTRY)                  *         
***********************************************************************         
                                                                                
         USING COMTABD,R2                                                       
VALPCT   NTR1                                                                   
         OC    RQPAROT,RQPAROT     IF ROTATION IS NOT PROVIDED                  
         JNZ   YES                                                              
         CLI   RQPAACT,ACTRES      AND ACTION IS NOT RESTORE ...                
         JE    YES                                                              
                                                                                
         MVI   ERRFLD,D#PAPCTS                                                  
         OC    CTPCT,CTPCT         IF PERCENTAGE IS NON-ZERO                    
         JZ    VPCT10              ENSURE REAL COMMERCIAL IS PROVIDED           
         CLC   CTCOM,=CL8'*'                                                    
         JE    ERR00006                                                         
         OC    CTCOM,CTCOM                                                      
         JZ    ERR00006                                                         
         J     YES                                                              
                                                                                
VPCT10   CLC   CTCOM,=CL8'*'       IF PERCENTAGE IS ZERO                        
         JNE   ERR00002            ENSURE COMMERCIAL IS BEING DELETED           
         J     YES                                                              
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        ROUTINE VALIDATES ROTATION                                   *         
*        ON ENTRY ... R3 = A(IOKEY)                                   *         
***********************************************************************         
                                                                                
VALROT   NTR1  BASE=*,LABEL=*                                                   
         CLI   ROTIND,0            IF ROTATION IS NOT PROVIDED                  
         JNE   VROT10                                                           
         BAS   RE,BLDDROT          BUILD DERIVED ROTATION                       
         J     XIT                                                              
                                                                                
VROT10   BAS   RE,VALIROT          IF ROTATION IS PROVIDED                      
         J     XIT                 VALIDATE ROTATION                            
                                                                                
***********************************************************************         
*        ROUTINE PROCESSES INPUT ROTATION                             *         
***********************************************************************         
                                                                                
VALIROT  NTR1                                                                   
         ZIC   R0,ROTIND           FOR EACH PROVIDED ROTATION LETTER            
         LA    R1,RQPAROT                                                       
                                                                                
         USING COMTABD,R2                                                       
VIR10    L     R2,ACOMTAB                                                       
VIR20    CLC   CTLET,0(R1)         ENSURE ROTATION LETTER IS ASSOCIATED         
         JE    VIR30               TO A COMMERCIAL                              
         CLI   CTLNQ(R2),X'FF'                                                  
         JE    ERR00769                                                         
         LA    R2,CTLNQ(R2)                                                     
         J     VIR20                                                            
                                                                                
VIR30    OI    CTSTAT,CTIROT       SET COMMERCIAL IS IN ROTATION                
                                                                                
         LA    R1,1(R1)            BUMP TO NEXT PATTERN LETTER                  
         JCT   R0,VIR10                                                         
         DROP  R2                                                               
                                                                                
***********************************************************************         
                                                                                
         USING COMTABD,R2                                                       
         L     R2,ACOMTAB          ENSURE EACH NON-DELETED COMMERCIAL           
VIR40    TM    CTSTAT,CTIROT       APPEARS IN PATTERN AT LEAST ONCE             
         JO    VIR50                                                            
         CLC   CTCOM,=CL8'*'                                                    
         JNE   ERR00089                                                         
VIR50    CLI   CTLNQ(R2),X'FF'                                                  
         JE    YES                                                              
         LA    R2,CTLNQ(R2)                                                     
         J     VIR40                                                            
         DROP  R2                                                               
                                                                                
***********************************************************************         
                                                                                
ERR00769 MVI   ERRFLD,D#PAROT                                                   
         LHI   RE,769              INVALID LETTER IN ROTATION                   
         MVC   ERRTXT,SPACES                                                    
         MVC   ERRTXT(L'ETXT769),ETXT769                                        
         MVC   ERRTXT+L'ETXT769(1),0(R1)                                        
         J     ERR0XIT                                                          
                                                                                
ETXT769  DC    C'Invalid letter in rotation - '                                 
                                                                                
***********************************************************************         
*        ROUTINE BUILDS DERIVED ROTATION                              *         
*        ON ENTRY ... R2 = A(USAGE TABLE)                             *         
***********************************************************************         
                                                                                
BLDDROT  NTR1                                                                   
         BAS   RE,STMAXMIN         SET MAXIMUM/MINIMUM PERCENTAGES              
                                                                                
         BAS   RE,STDIVISR         TRY TO SET DIVISOR (COMDIV)                  
         JE    BDR10                                                            
                                                                                
         CLI   SVTN2PRO+9,C'Y'     IF WE CAN'T AND T2 PROFILE ALLOWS            
         JNE   ERR00638            US TO ADJUST PERCENTAGES                     
         BAS   RE,ADJPCTS          ADJUST THE PERCENTAGES                       
                                                                                
BDR10    XC    TEMP1,TEMP1                                                      
                                                                                
         USING COMTABD,R3                                                       
         L     R3,ACOMTAB          R3 = A(COMMERCIAL TABLE)                     
         LA    R4,TEMP1            R4 = A(ROTATION TABLE)                       
         L     R6,COMDIV           R6 = DIVISOR                                 
         XR    R1,R1               CLEAR R1 AND RF FOR DIVISION                 
         XR    RF,RF                                                            
                                                                                
BDR20    MVC   0(1,R4),CTLET       MOVE COM LETTER INTO ROTATION TABLE          
                                                                                
         ZICM  RF,CTPCT,3          DIVIDE COMMERCIAL PERCENTAGE                 
         XR    RE,RE               BY DIVISOR                                   
         DR    RE,R6               STORE ITERATIONS (QUOTIENT)                  
         STCM  RF,3,1(R4)          INTO TABLE                                   
                                                                                
         AR    R1,RF               ENSURE TOTAL NUMBER OF ITERATIONS            
         CHI   R1,60               DOES NOT EXCEED 59                           
         JNL   ERR00638                                                         
                                                                                
         LA    R4,3(,R4)           BUMP TO NEXT TABLE ENTRY                     
         LA    R3,CTLNQ(R3)        BUMP TO NEXT COMMERCIAL                      
         CLI   0(R3),X'FF'                                                      
         JNE   BDR20                                                            
         STC   R1,DROTLEN          SAVE LENGTH OF DERIVED ROTATION              
         DROP  R3                                                               
                                                                                
***********************************************************************         
                                                                                
         LA    R4,DROT             R4 = A(DERIVED ROTATION)                     
BDR30    ZIC   R0,COMIND           R0 = NUMBER OF COMMERCIALS                   
         LA    R3,TEMP1            R3 = A(ROTATION TABLE)                       
                                                                                
BDR40    ZICM  RE,1(R3),2          IF COM LETTER HAS ANY ITERATIONS             
         JZ    BDR50               REMAINING                                    
         MVC   0(1,R4),0(R3)       MOVE IT INTO DERIVED ROTATION                
         BCTR  RE,0                                                             
         STCM  RE,3,1(R3)          AND DECREMENT REMAINING ITERATIONS           
                                                                                
         BCTR  R1,0                DECREMENT TOTAL NUMBER OF ITERATIONS         
         LTR   R1,R1               IF NONE LEFT, DONE                           
         JZ    YES                                                              
                                                                                
         LA    R4,1(R4)            BUMP TO NEXT POSITION IN DERIVED ROT         
BDR50    LA    R3,3(R3)            BUMP TO NEXT ROTATION TABLE ENTRY            
         JCT   R0,BDR40            IF ANY COMS LEFT, DO NEXT COM                
         J     BDR30                                                            
                                                                                
***********************************************************************         
*        ROUTINE SETS MAXIMUM (MAXPCT) AND MINIMUM (MINPCT)           *         
*        PERCENTAGES                                                  *         
***********************************************************************         
                                                                                
STMAXMIN NTR1                                                                   
         MVI   MINPCT+3,X'FF'                                                   
                                                                                
         ZIC   RE,PCTIND                                                        
         ZICM  RF,PCTS,3                                                        
                                                                                
SMM10    CLC   MAXPCT+1(3),0(RF)                                                
         JNL   SMM20                                                            
         MVC   MAXPCT+1(3),0(RF)   SET MAXIMUM                                  
                                                                                
SMM20    CLC   MINPCT+1(3),0(RF)                                                
         JNH   SMM30                                                            
         MVC   MINPCT+1(3),0(RF)   AND MINIMUM PERCENTAGE                       
                                                                                
SMM30    LA    RF,3(RF)                                                         
         JCT   RE,SMM10                                                         
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE SETS DIVISOR (COMDIV)                                *         
***********************************************************************         
                                                                                
STDIVISR NTR1                                                                   
         GOTO1 CKD,DMCB,MINPCT     IF ALL PCTS DIVIDE EVENLY BY                 
         JE    YES                 MIN PCT, MIN PCT IS OUR DIVISOR              
                                                                                
         TM    MINPCT+3,1          IF MINIMUM PERCENTAGE IS AN                  
         JO    SDIV10              EVEN NUMBER                                  
         L     R0,MINPCT           AND ALL PCTS DIVIDE EVENLY BY                
         SRL   R0,1                MIN PCT/2, MIN PCT/2 IS OUR DIVISOR          
         ST    R0,COMDIV                                                        
         GOTO1 CKD,DMCB,COMDIV                                                  
         JE    YES                                                              
                                                                                
SDIV10   LA    R2,SDPRIMES         IF ALL PERCENTAGES DIVIDE BY                 
SDIV20   GOTO1 CKD,DMCB,0(R2)      AN ENTRY IN OUR PRIME NUMBER TABLE           
         JE    SDIV30                                                           
         CLI   L'SDPRIMES(R2),X'FF'                                             
         JE    NO                                                               
         LA    R2,L'SDPRIMES(R2)                                                
         J     SDIV20                                                           
                                                                                
SDIV30   L     R0,COMDIV           KEEP DOUBLING THAT PRIME NUMBER              
         AR    R0,R0               UNTIL IT DOESN'T DIVIDE EVENLY               
         ST    R0,COMDIV                                                        
         GOTO1 CKD,DMCB,COMDIV                                                  
         JE    SDIV30                                                           
         L     R0,COMDIV                                                        
         SRL   R0,1                                                             
         ST    R0,COMDIV                                                        
         J     YES                                                              
                                                                                
SDPRIMES DS    0F                                                               
         DC    F'2'                                                             
         DC    F'3'                                                             
         DC    F'5'                                                             
         DC    F'7'                                                             
         DC    F'11'                                                            
         DC    F'13'                                                            
         DC    F'17'                                                            
         DC    F'19'                                                            
         DC    F'23'                                                            
         DC    F'29'                                                            
         DC    F'31'                                                            
         DC    F'37'                                                            
         DC    X'FF'                                                            
                                                                                
***********************************************************************         
*        ROUTINE CHECKS SUPPLIED DIVSOR AGAINST ALL PERCENTAGES       *         
*        ON ENTRY ... COMDIV = SUPPLIED DIVISOR                       *         
***********************************************************************         
                                                                                
CKD      NTR1                                                                   
         L     R1,0(R1)                                                         
         MVC   COMDIV,0(R1)        COMDIV = SUPPLIED DIVISOR                    
                                                                                
         XR    R0,R0                                                            
         XR    R1,R1                                                            
                                                                                
         ZIC   RF,PCTIND           RF=# OF PERCENTAGES                          
         ZICM  R3,PCTS,3           R3=A(PERCENTAGES)                            
CKD10    ZICM  R1,0(R3),3                                                       
         D     R0,COMDIV                                                        
         LTR   R0,R0               IF ANY DO NOT DIVIDE EVENLY                  
         JNZ   NO                  RETURN NEGATIVE CONDITION CODE               
         LA    R3,3(R3)                                                         
         JCT   RF,CKD10                                                         
                                                                                
         CR    R0,R0                                                            
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE ADJUSTS SUPPLIED PERCENTAGES                         *         
***********************************************************************         
                                                                                
ADJPCTS  NTR1                                                                   
         OI    PROSTAT,PSPCTADJ    SET PERCENTAGES ADJUSTED                     
                                                                                
***********************************************************************         
                                                                                
         LA    R0,APTLNQ           R0=L'QUOTIENT TABLE                          
         LA    R1,APTAB            R1=A(QUOTIENT TABLE)                         
         LA    R2,RELTAB           R2=A(RELTAB)                                 
                                                                                
         USING COMTABD,R6                                                       
APCTS10  ZIC   R3,PCTIND                                                        
         XR    R4,R4                                                            
         L     R6,ACOMTAB                                                       
APCTS20  XR    RE,RE               DIVIDE EACH PERCENTAGE                       
         ZICM  RF,CTPCT,3          BY QUOTIENT TABLE ENTRY                      
         D     RE,0(R1)                                                         
         AR    R4,RE               ADD REMAINDER TO REMAINDER                   
         LA    R6,CTLNQ(R6)        TOTAL                                        
         JCT   R3,APCTS20                                                       
         DROP  R6                                                               
                                                                                
         XR    RE,RE                                                            
         LR    RF,R4               DIVIDE TOTAL REMAINDER BY                    
         D     RE,0(R1)            QUOTIENT TABLE ENTRY                         
         STC   RE,1(R2)            STORE REMAINDER                              
         STC   RF,0(R2)            AND QUOTIENT INTO RELTAB                     
                                                                                
         LA    R1,4(R1)            BUMP TO NEXT QUOTIENT TABLE ENTRY            
         LA    R2,2(R2)            AND RELTAB ENTRY                             
         JCT   R0,APCTS10                                                       
                                                                                
***********************************************************************         
                                                                                
         LA    R0,APTLNQ           R0=L'QUOTIENT TABLE                          
         LA    R1,APTAB            R1=A(QUOTIENT TABLE)                         
         LA    R2,RELTAB           R2=A(RELTAB)                                 
         LA    R3,2047                                                          
         XR    R4,R4                                                            
                                                                                
APCTS30  CLM   R3,3,0(R2)                                                       
         JNH   APCTS40                                                          
         LR    R4,R1               SAVE LOWEST QUOTIENT REMAINDER               
         ICM   R3,3,0(R2)          INTO COMDIV                                  
APCTS40  LA    R1,4(R1)                                                         
         LA    R2,2(R2)                                                         
         JCT   R0,APCTS30                                                       
         MVC   COMDIV,0(R4)                                                     
                                                                                
***********************************************************************         
                                                                                
         USING COMTABD,R2                                                       
         ZIC   R0,PCTIND           R0=# OF PERCENTAGES                          
         L     R2,ACOMTAB          R2=A(COMMERCIAL TABLE)                       
         LA    R3,REMTAB           R3=A(REMAINDER TABLE)                        
APCTS50  XR    RE,RE                                                            
         ZICM  RF,CTPCT,3                                                       
         D     RE,0(R4)                                                         
         STCM  RF,7,CTPCT          STORE QUOTIENT                               
         ST    RE,0(R3)            & REMAINDER                                  
         LA    R2,CTLNQ(R2)                                                     
         LA    R3,4(R3)                                                         
         BCT   R0,APCTS50                                                       
         DROP  R2                                                               
                                                                                
         USING COMTABD,R2                                                       
         ZIC   R0,PCTIND                                                        
         L     R2,ACOMTAB                                                       
APCTS60  ZICM  RE,CTPCT,3                                                       
         MH    RE,2(R4)                                                         
         STCM  RE,7,CTPCT          STORE RESULT                                 
         LA    R2,CTLNQ(R2)                                                     
         JCT   R0,APCTS60                                                       
         DROP  R2                                                               
                                                                                
         USING COMTABD,R2                                                       
APCTS70  ZIC   R0,PCTIND           SEE IF SUM 99 OR 100                         
         L     R2,ACOMTAB                                                       
         SR    RF,RF                                                            
APCTS80  ZICM  RE,CTPCT,3                                                       
         AR    RF,RE                                                            
         LA    R2,CTLNQ(R2)                                                     
         JCT   R0,APCTS80                                                       
         DROP  R2                                                               
                                                                                
         CHI   RF,100                                                           
         JE    XIT                                                              
         CHI   RF,99                                                            
         JE    XIT                                                              
                                                                                
         USING COMTABD,R2                                                       
         ZIC   R0,PCTIND                                                        
         L     R2,ACOMTAB                                                       
         LA    R3,REMTAB                                                        
         XR    R1,R1                                                            
APCTS90  C     R1,0(R3)                                                         
         JNL   APCTS100                                                         
         LR    RE,R2                                                            
         LR    RF,R3                                                            
         L     R1,0(R3)                                                         
APCTS100 LA    R2,CTLNQ(R2)                                                     
         LA    R3,4(R3)                                                         
         JCT   R0,APCTS90                                                       
         DROP  R2                                                               
                                                                                
         ZICM  R0,CTPCT-COMTABD(RE),3                                           
         A     R0,0(R4)                                                         
         STCM  R0,7,CTPCT-COMTABD(RE)                                           
         XC    0(4,RF),0(RF)                                                    
         J     APCTS70                                                          
                                                                                
APTAB    DS    0F                                                               
         DC    F'2'                                                             
         DC    F'3'                                                             
         DC    F'5'                                                             
         DC    F'11'                                                            
APTLNQ   EQU   (*-APTAB)/4                                                      
                                                                                
***********************************************************************         
*        ROUTINE HANDLES VALIDATION OF PATTERN RECORD FOR             *         
*        DELETE/RESTORE                                               *         
*        ON ENTRY ... R3 = A(IOKEY)                                   *         
***********************************************************************         
                                                                                
VPATDR   NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,AREQDR           ASSERT REQUIRED/NOT ALLOWED RULES            
         JNE   NO                  ARE SATISIFIED FOR DELETE/RESTORE            
                                                                                
         BRAS  RE,VPATALL          VALIDATE PATTERN RECORD FOR                  
         JNE   NO                  ALL ACTIONS                                  
                                                                                
         BRAS  RE,VALREF           VALIDATE REFERENCE NUMBER                    
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE HANDLES VALIDATION OF PATTERN RECORD FOR             *         
*        ADD/CHANGE/DELETE/RESTORE                                    *         
*        ON ENTRY ... R3 = A(IOKEY)                                   *         
***********************************************************************         
                                                                                
VPATALL  NTR1  BASE=*,LABEL=*                                                   
         BAS   RE,ASRTVAL          ASSERT ALL PROVIDED VALUES                   
         JNE   NO                  ARE VALID                                    
                                                                                
         GOTOR (#VALMED,AVALMED),DMCB,=C'N',0,QMEDX                             
         JE    *+6                 ASSUME MEDIA NETWORK                         
         DC    H'00'                                                            
                                                                                
         BAS   RE,VALCLT           VALIDATE CLIENT                              
         JNE   NO                                                               
                                                                                
         BAS   RE,VALNMD           VALIDATE NETWORK MEDIA                       
         JNE   NO                                                               
                                                                                
         BAS   RE,VALNET           VALIDATE NETWORK                             
         JNE   NO                                                               
                                                                                
         BAS   RE,VALPRO           VALIDATE PROGRAM                             
         JNE   NO                                                               
                                                                                
         BAS   RE,VALDPT           VALIDATE DAYPART CODE                        
         JNE   NO                                                               
                                                                                
         BAS   RE,VALFEED          VALIDATE FEED                                
         JNE   NO                                                               
                                                                                
         BAS   RE,VALPRD           VALIDATE PRODUCT-LENGTH                      
         JNE   NO                                                               
                                                                                
         BAS   RE,VALPPR           VALIDATE PARTNER PRODUCT-LENGTH              
         JNE   NO                                                               
                                                                                
         BAS   RE,VALPCTS          VALIDATE PERCENTAGES                         
         JNE   NO                                                               
                                                                                
         BAS   RE,VALUAP           VALIDATE UPDATING APPLICATION                
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE ASSERT ALL PROVIDED VALUES ARE VALID                 *         
***********************************************************************         
                                                                                
ASRTVAL  NTR1                                                                   
         CLI   RQPANMD,0           IF NETWORK MEDIA IS PROVIDED                 
         JE    AV20                                                             
         LA    R1,VALMEDS                                                       
AV10     CLC   RQPANMD,0(R1)       ASSERT THAT VALUE IS VALID                   
         JE    AV20                                                             
         CLI   1(R1),X'FF'                                                      
         JE    ERR00607                                                         
         LA    R1,1(R1)                                                         
         J     AV10                                                             
                                                                                
AV20     MVI   ERRFLD,D#PANET                                                   
         CLC   RQPANET,=CL4'ALL'   NETWORK CANNOT BE ALL                        
         JE    ERR00503                                                         
                                                                                
         OC    RQPAPPR,RQPAPPR     IF PRODUCT PARTNER IS PROVIDED               
         JZ    AV30                                                             
         MVI   ERRFLD,D#PAPPR                                                   
         CLC   RQPAPRD,RQPAPPR     MUST BE ALPHABETICALLY HIGHER                
         JH    ERR00088            THAN PRODUCT                                 
                                                                                
         CLI   RQPAUFN,0           ASSERT THAT UFN DATE IS BLANK                
         JE    AV30                                                             
         MVI   ERRFLD,D#PAUFN                                                   
         CLI   RQPAUFN,C'Y'        OR Y                                         
         JNE   ERR00002                                                         
                                                                                
AV30     OC    RQPAPEN,RQPAPEN     IF PERIOD END DATE IS PROVIDED               
         JZ    AV40                                                             
         MVI   ERRFLD,D#PAPEN                                                   
         CLC   RQPAPST,RQPAPEN     ASSERT END DATE IS LATER THAN                
         JH    ERR00002            PERIOD START DATE                            
                                                                                
AV40     OC    RQPASTM,RQPASTM     IF START TIME IS PROVIDED                    
         JZ    AV50                ENSURE IT IS VALID TIME                      
         GOTOR SETLEN,DMCB,BYTE,RQPASTM,L'RQPASTM                               
         GOTOR VTIMVAL,DMCB,(BYTE,RQPASTM),RQPASTMX                             
         MVI   ERRFLD,D#PASTM                                                   
         CLI   0(R1),X'FF'                                                      
         JE    ERR00002                                                         
         GOTOR ADJTIM,DMCB,RQPASTMX,RQPASTMA                                    
                                                                                
         OC    RQPAETM,RQPAETM     IF END TIME IS PROVIDED                      
         JZ    AV50                ENSURE IT IS VALID TIME                      
         GOTOR SETLEN,DMCB,BYTE,RQPAETM,L'RQPAETM                               
         GOTOR VTIMVAL,DMCB,(BYTE,RQPAETM),RQPAETMX                             
         MVI   ERRFLD,D#PAETM                                                   
         CLI   0(R1),X'FF'                                                      
         JE    ERR00002                                                         
         CLC   RQPAETMX,=H'2400'                                                
         JNL   ERR00002                                                         
         GOTOR ADJTIM,DMCB,RQPAETMX,RQPAETMA                                    
                                                                                
***********************************************************************         
                                                                                
         CLC   RQPAPST,RQPAPEN     IF PATTERN COVERS JUST ONE DAY               
         JNE   AV50                                                             
                                                                                
         LH    R0,RQPASTMX         MASSAGE START TIME                           
         J     *+16                                                             
         CHI   R0,599                                                           
         JH    *+8                                                              
         AHI   R0,2400                                                          
         CHI   R0,2400                                                          
         JNE   *+6                                                              
         XR    R0,R0                                                            
                                                                                
         LH    R1,RQPAETMX         MASSAGE END TIME                             
         J     *+16                                                             
         CHI   R1,599                                                           
         JH    *+8                                                              
         AHI   R1,2400                                                          
                                                                                
         MVI   ERRFLD,D#PAETM                                                   
         CR    R0,R1               ENSURE START TIME IS EARLIER                 
         JH    ERR00002            THAN END TIME                                
                                                                                
***********************************************************************         
                                                                                
AV50     MVI   ERRFLD,D#PANETS                                                  
         CLI   NETIND,26           ASSERT NO MORE THAN 26 NETWORKS              
         JH    ERR00002            ARE PROVIDED                                 
                                                                                
         CLI   NETIND,2                                                         
         JL    AV80                                                             
         ZIC   RE,NETIND           ASSERT NO NETWORKS ARE DUPLICATED            
         BCTR  RE,0                                                             
         ZICM  RF,NETS,3                                                        
AV60     LR    R0,RE                                                            
         LA    R1,4(RF)                                                         
AV70     CLC   0(4,RF),0(R1)                                                    
         JE    ERR00002                                                         
         LA    R1,4(R1)                                                         
         JCT   R0,AV70                                                          
         LA    RF,4(RF)                                                         
         JCT   RE,AV60                                                          
                                                                                
***********************************************************************         
                                                                                
AV80     MVI   ERRFLD,D#PACOMS                                                  
         CLI   COMIND,12           ASSERT NO MORE THAN 12 COMMERCIALS           
         JH    ERR00002            ARE PROVIDED                                 
                                                                                
         CLI   PIGINDP,0           IF PIGGYBACK COMMERCIALS ARE                 
         JE    AV90                PROVIDED                                     
         MVI   ERRFLD,D#PAPIGS                                                  
         CLC   PIGIND,COMIND       ASSERT NUMBER OF PIGGYBACK COMMLS            
         JNE   ERR00002            MATCHES NUMBER OF COMMERCIALS                
                                                                                
***********************************************************************         
                                                                                
AV90     CLI   COMIND,1            IF MORE THAN 1 COMMERCIAL IS                 
         JNH   AV140               PROVIDED                                     
                                                                                
         ZIC   R0,COMIND           R0=# OF COMMERCIAL                           
         BCTR  R0,0                                                             
         ZICM  R1,COMS,3           R1=A(COMMERCIALS)                            
         XR    R2,R2                                                            
         CLI   PIGIND,0                                                         
         JE    AV100                                                            
         ZICM  R2,PIGS,3           R2=A(PIGGYBACK COMMERCIALS)                  
                                                                                
AV100    LR    R3,R0               R3=(# OF COMM'LS BEYOND CURRENT)             
         LA    R4,12(R1)           R4=A(NEXT COMMERCIAL)                        
         LA    R6,12(R2)           R6=A(NEXT PIGGYBACK COMMERCIAL)              
AV110    CLC   0(12,R1),0(R4)      IF COMMERCIAL IS DUPLICATED                  
         JNE   AV120                                                            
         CLC   0(12,R1),=CL12'*'                                                
         JE    AV120                                                            
         MVI   ERRFLD,D#PACOMS                                                  
         LTR   R2,R2               RETURN ERROR IF NO PIGGYBACK                 
         JZ    ERR00002            COMMERCIAL                                   
         OC    0(12,R2),0(R2)                                                   
         JZ    ERR00002                                                         
         MVI   ERRFLD,D#PAPIGS                                                  
         CLC   0(12,R2),0(R6)      OR IF PIGGYBACK COMMERCIAL MATCHES           
         JE    ERR00002            AS WELL                                      
AV120    LA    R4,12(R4)                                                        
         LA    R6,12(R6)           KEEP BUMPING COMMERCIAL/PIGGYBACK            
         JCT   R3,AV110            UNTIL WE REACH END OF LIST                   
                                                                                
         LA    R1,12(R1)                                                        
         LTR   R2,R2                                                            
         JZ    AV130                                                            
         LA    R2,12(R2)           KEEP BUMPING COMMERCIAL/PIGGYBACK            
AV130    JCT   R0,AV100            UNTIL WE REACH END OF LIST                   
                                                                                
***********************************************************************         
                                                                                
AV140    CLI   PCTINDP,0           IF PERCENTAGES ARE PROVIDED                  
         JE    AV160               PROVIDED                                     
         MVI   ERRFLD,D#PAPCTS                                                  
         CLC   PCTINDP,COMINDP     ASSERT NUMBER OF PERCENTAGES                 
         JNE   ERR00002            MATCHES NUMBER OF COMMERCIALS                
                                                                                
         ZIC   RE,PCTIND                                                        
         ZICM  RF,PCTS,3           ENSURE SUM OF ALL PERCENTAGES IS             
         XR    R1,R1               99 OR 100                                    
AV150    ZICM  R2,0(RF),3                                                       
         AR    R1,R2                                                            
         LA    RF,3(RF)                                                         
         JCT   RE,AV150                                                         
         CHI   R1,99                                                            
         JE    AV160                                                            
         CHI   R1,100                                                           
         JE    AV160                                                            
         MVI   ERRFLD,D#PAPCTS                                                  
         J     ERR00002                                                         
                                                                                
***********************************************************************         
                                                                                
         USING HIATABD,R2                                                       
AV160    L     R2,AHIATAB                                                       
AV170    CLI   0(R2),X'FF'         FOR EACH ENTRY IN HIATUS TABLE ...           
         JE    AV210                                                            
                                                                                
         OC    HTDAY,HTDAY         ENSURE DAY IS VALID                          
         JZ    AV180                                                            
         GOTOR SETLEN,DMCB,BYTE,HTDAY,L'HTDAY                                   
         GOTOR VDAYVAL,DMCB,(BYTE,HTDAY),FULL,BYTE4                             
         MVI   ERRFLD,D#PAHDYS                                                  
         CLI   FULL,0                                                           
         JE    ERR00002                                                         
         CLI   FULL,X'7F'                                                       
         JE    ERR00002                                                         
         XC    HTDAY,HTDAY                                                      
         MVC   HTDAYX,FULL                                                      
         MVC   HTSEQ,BYTE4                                                      
                                                                                
AV180    OC    HTDAT,HTDAT                                                      
         JZ    AV190                                                            
         MVI   ERRFLD,D#PAHDTS                                                  
         CLC   RQPAPST,HTDAT      ENSURE PERIOD START DATE IS NOT LATER         
         JH    ERR00002           THAN HIATUS DATE                              
         CLC   RQPAPEN,HTDAT      AND PERIOD END DATE IS NOT EARLIER            
         JL    ERR00002           THAN HIATUS DATE                              
                                                                                
AV190    OC    HTSTM,HTSTM        ENSURE START TIME IS VALID                    
         JZ    AV200                                                            
         MVI   ERRFLD,D#PAHSTS                                                  
         GOTOR SETLEN,DMCB,BYTE,HTSTM,L'HTSTM                                   
         GOTOR VTIMVAL,DMCB,(BYTE,HTSTM),WORK                                   
         CLI   0(R1),X'FF'                                                      
         JE    ERR00002                                                         
         XC    HTSTM,HTSTM                                                      
         MVC   HTSTMX,WORK                                                      
                                                                                
         MVI   ERRFLD,D#PAHETS    ENSURE END TIME IS VALID                      
         GOTOR SETLEN,DMCB,BYTE,HTETM,L'HTETM                                   
         GOTOR VTIMVAL,DMCB,(BYTE,HTETM),WORK                                   
         CLI   0(R1),X'FF'                                                      
         JE    ERR00002                                                         
         XC    HTETM,HTETM                                                      
         MVC   HTETMX,WORK                                                      
                                                                                
         CLC   HTSTMX,HTETMX       ENSURE START AND END TIME ARE                
         JE    ERR00002            NOT EQUAL                                    
         JL    AV200                                                            
         CLC   HTETMX,=H'600'                                                   
         JNL   ERR00002                                                         
                                                                                
AV200    LA    R2,HTLNQ(R2)                                                     
         J     AV170                                                            
         DROP  R2                                                               
                                                                                
***********************************************************************         
                                                                                
         USING HIATABD,R2                                                       
AV210    L     R2,AHIATAB                                                       
AV220    CLI   0(R2),X'FF'         FOR EACH ENTRY IN HIATUS TABLE ...           
         JE    YES                                                              
                                                                                
         MVI   ERRFLD,D#PAHSTS                                                  
         LA    RE,HTLNQ(R2)        IF ANY DAYS/DATES ARE DUPLICATED             
AV230    CLI   0(RE),X'FF'         ENSURE BOTH HAVE TIMES PROVIDED              
         JE    AV260                                                            
         CLC   0(L'HTDAY+L'HTDAT,R2),0(RE)                                      
         JNE   AV250                                                            
         OC    HTSTM,HTSTM                                                      
         JZ    ERR00807                                                         
         OC    HTSTM-HIATABD(L'HTSTM,RE),HTSTM-HIATABD(RE)                      
         JZ    ERR00807                                                         
                                                                                
         MVC   DUB(2),HTSTMX                                                    
         MVC   DUB+2(2),HTETMX                                                  
         MVC   DUB+4(2),HTSTMX-HIATABD(RE)                                      
         MVC   DUB+6(2),HTETMX-HIATABD(RE)                                      
         LA    R1,DUB                                                           
         LHI   R0,4                                                             
AV240    CLC   0(2,R1),=H'2400'    AND ENSURE TIMES DO NOT OVERLAP              
         JNE   *+10                                                             
         XC    0(2,R1),0(R1)                                                    
         LA    R1,2(R1)                                                         
         BCT   R0,AV240                                                         
         CLC   DUB(2),DUB+6                                                     
         JH    AV250                                                            
         CLC   DUB+2(2),DUB+4                                                   
         JL    AV250                                                            
         CLI   HDYINDP,0                                                        
         JNE   ERR00807                                                         
         J     ERR00808                                                         
                                                                                
AV250    LA    RE,HTLNQ(RE)                                                     
         J     AV230                                                            
                                                                                
AV260    LA    R2,HTLNQ(R2)                                                     
         J     AV220                                                            
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        CONSTANTS                                                    *         
***********************************************************************         
                                                                                
VALMEDS  DC    C'C'                CABLE ONLY                                   
         DC    C'D'                NETWORK RADIO                                
         DC    C'H'                HISPANIC                                     
         DC    C'N'                NETWORK                                      
         DC    C'O'                OTHER                                        
         DC    C'S'                SYNDICATION                                  
         DC    C'V'                DIGITAL                                      
         DC    X'FF'                                                            
                                                                                
***********************************************************************         
*        ROUTINE PROCESSES CLIENT                                     *         
***********************************************************************         
                                                                                
VALCLT   NTR1                                                                   
         GOTOR (#VALCLT,AVALCLT),DMCB,RQPACLT,L'RQPACLT-1,QCLTX                 
         JNE   ERR00014                                                         
                                                                                
         USING CLTHDR,R2                                                        
         L     R2,AIO2                                                          
         XC    WORK,WORK                                                        
         MVI   WORK,X'A2'          READ FOR CLIENT'S TN2 PROFILE                
         MVC   WORK+1(3),=C'TN2'   AND SAVE IT INTO SVTN2PRO                    
         MVI   WORK+3,C'2'                                                      
         MVC   WORK+4(2),SVLPAGY                                                
         MVI   WORK+6,C'N'                                                      
         MVC   WORK+7(3),RQPACLT                                                
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),CTRAFOFC                                              
         CLI   WORK+11,C' '                                                     
         JNL   *+10                                                             
         MVC   WORK+11(1),COFFICE                                               
         GOTO1 GETPROF,DMCB,WORK,SVTN2PRO,DATAMGR                               
         DROP  R2                                                               
                                                                                
         MVI   WORK+3,C'1'         READ FOR CLIENT'S TN1 PROFILE                
         GOTO1 (RF),(R1),,SVTN1PRO AND SAVE IT INTO SVTN1PRO                    
                                                                                
         MVI   WORK+3,C'3'         READ FOR CLIENT'S TN3 PROFILE                
         GOTO1 (RF),(R1),,SVTN3PRO AND SAVE IT INTO SVTN3PRO                    
         J     YES                                                              
                                                                                
***********************************************************************         
*        ROUTINE PROCESSES NETWORK MEDIA                              *         
***********************************************************************         
                                                                                
VALNMD   NTR1                                                                   
         CLI   RQPANMD,0           IF NETWORK MEDIA IS PROVIDED                 
         JE    YES                                                              
                                                                                
         CLI   RQPANMD,C'V'        IF NETWORK MEDIA IS V (DIGITAL)              
         JNE   PNM10                                                            
         MVI   ERRFLD,D#PANMD                                                   
         CLI   SVTN2PRO+14,C'Y'    TN2 PROFILE MUST ALLOW IT                    
         JE    ERR00590                                                         
                                                                                
PNM10    MVC   NETWORK+1(1),RQPANMD                                             
         MVI   NETWORK+2,X'FF'     SET NETWORK CODE                             
         J     YES                                                              
                                                                                
***********************************************************************         
*        ROUTINE PROCESSES NETWORK                                    *         
*        ON ENTRY ... R3 = A(IOKEY)                                   *         
***********************************************************************         
                                                                                
VALNET   NTR1                                                                   
         OC    RQPANET,RQPANET     IF NETWORK IS PROVIDED ...                   
         JZ    YES                                                              
                                                                                
         MVI   ERRFLD,D#PANET                                                   
         GOTOR READNET,DMCB,RQPANET                                             
         JNE   NO                  ENSURE NETWORK EXISTS                        
                                                                                
         USING STARECD,R4                                                       
         L     R4,AIO4                                                          
         PACK  DUB,SMKT                                                         
         CVB   R0,DUB                                                           
         STH   R0,NETMKT           SAVE NETWORK MARKET NUMBER                   
         MVC   NETWORK,RQPANET     AND SET NETWORK CODE                         
         J     YES                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE READS FOR PROVIDED NETWORK                           *         
*        ON ENTRY ... R3 = A(IOKEY)                                   *         
*                     P1 = A(NETWORK)                                 *         
***********************************************************************         
                                                                                
READNET  NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)            R2=A(NETWORK)                                
                                                                                
         USING STARECD,R3                                                       
         MVC   IOKEY(17),=17C'0'                                                
         MVI   STAKTYPE,C'S'                                                    
         MVI   STAKMED,C'N'        ENSURE STATION EXISTS                        
         MVC   STAKCALL(4),0(R2)                                                
         MVI   STAKCALL+4,C'N'                                                  
         MVC   STAKAGY,SVLPAGY                                                  
         MVC   STAKCLT,QCLTX                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOSTAFIL+IO4'                            
         JNE   ERR00503                                                         
         DROP  R3                                                               
                                                                                
         USING STARECD,R4                                                       
         L     R4,AIO4                                                          
         CLI   STRTYPE,C'V'        IF STATION IS TYPE V (DIGITAL)               
         JNE   YES                                                              
         CLI   SVTN2PRO+14,C'Y'    TN2 PROFILE MUST ALLOW IT                    
         JE    ERR00590                                                         
         J     YES                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE PROCESSES PROGRAM                                    *         
*        ON ENTRY ... R3 = A(IOKEY)                                   *         
***********************************************************************         
                                                                                
VALPRO   NTR1                                                                   
         OC    RQPAPRO,RQPAPRO     IF PROGRAM IS PROVIDED ...                   
         JZ    YES                                                              
         MVI   ERRFLD,D#PAPRO                                                   
                                                                                
         USING NPGRECD,R3                                                       
         XC    NPGKEY,NPGKEY                                                    
         MVC   NPGKTYP,=X'0D20'    ENSURE THAT PROGRAM EXISTS                   
         MVC   NPGKAM,QMEDX                                                     
         MVC   NPGKNET,NETMKT                                                   
         MVC   NPGKPROG,RQPAPRO                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOSPTDIR+IO3'                            
         CLC   IOKEY(11),IOKEYSAV                                               
         JNE   ERR00502                                                         
         DROP  R3                                                               
                                                                                
         USING PGEKEY,R3                                                        
         XC    PGEKEY,PGEKEY       ENSURE THAT PROGRAM EQUIVALENCY              
         MVI   PGEKID,X'24'        DOES NOT EXIST                               
         MVC   PGEKAM,QMEDX                                                     
         MVC   PGEKCLT,QCLTX                                                    
         MVC   PGEKNET,NETWORK                                                  
         MVC   PGEKPRG,RQPAPRO                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOUNTDIR+IO3'                            
         CLC   IOKEY(14),IOKEYSAV                                               
         JE    ERR00560                                                         
         DROP  R3                                                               
                                                                                
         USING NPGRECD,R3                                                       
         XC    NPGKEY,NPGKEY                                                    
         MVC   NPGKTYP,=X'0D20'    ENSURE THAT PROGRAM IS COVERED               
         MVC   NPGKAM,QMEDX        FOR PERIOD START/END DATE                    
         MVC   NPGKNET,NETMKT                                                   
         MVC   NPGKPROG,RQPAPRO                                                 
         GOTO1 DATCON,DMCB,(3,RQPAPST),(2,NPGKEND)                              
         CLI   RQPAUFN,C'Y'                                                     
         JE    VPRO10                                                           
         GOTO1 DATCON,DMCB,(3,RQPAPEN),(2,NPGKEND)                              
VPRO10   GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOSPTDIR+IO3'                            
         CLC   NPGKEY(NPGKEND-NPGKEY),IOKEYSAV                                  
         JNE   ERR00631                                                         
         J     YES                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        ROUTINE VALIDATES DAYPART CODE                               *         
*        ON ENTRY ... R3 = A(IOKEY)                                   *         
***********************************************************************         
                                                                                
VALDPT   NTR1                                                                   
         OC    RQPADPT,RQPADPT                                                  
         JZ    YES                                                              
         GOTOR (#VALDPT,AVALDPT),DMCB,(0,RQPADPT)                               
         JNE   ERR00021                                                         
         J     YES                                                              
                                                                                
***********************************************************************         
*        ROUTINE VALIDATES FEED                                       *         
*        ON ENTRY ... R3 = A(IOKEY)                                   *         
***********************************************************************         
                                                                                
VALFEED  NTR1                                                                   
         OC    RQPAFED,RQPAFED     IF FEED IS PROVIDED ...                      
         JZ    YES                                                              
                                                                                
         USING FEEDKEY,R3                                                       
         XC    FEEDKEY,FEEDKEY                                                  
         MVC   FEEDKID,=X'0A2B'    ENSURE IT EXISTS UNDER CLIENT                
         MVC   FEEDKAM,QMEDX                                                    
         MVC   FEEDKNET,NETWORK                                                 
         MVC   FEEDKCLT,QCLTX                                                   
         MVC   FEEDKFD,RQPAFED                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOSPTDIR+IO3'                            
         JE    YES                                                              
                                                                                
         MVC   IOKEY(L'FEEDKEY),IOKEYSAV                                        
         XC    FEEDKCLT,FEEDKCLT   OR AT GLOBAL CLIENT LEVEL                    
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOSPTDIR+IO3'                            
         JNE   ERR00628                                                         
         J     YES                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        ROUTINE VALIDATES PRODUCT-LENGTH                             *         
*        ON ENTRY ... R3 = A(IOKEY)                                   *         
***********************************************************************         
                                                                                
VALPRD   NTR1                                                                   
         OC    RQPAPRL,RQPAPRL     IF PRODUCT LENGTH IS NOT PROVIDED            
         JNZ   PPRD10                                                           
         MVI   RQPAPRL,30          DEFAULT TO 30                                
                                                                                
PPRD10   GOTOR (#VALPRD,AVALPRD),DMCB,RQPAPRD,RQPAPRL                           
         JNE   PPRD20                                                           
         MVC   QPROD1X,QPRODX      VALIDATE PRODUCT AND LENGTH                  
         J     YES                                                              
                                                                                
PPRD20   CLI   DMCB,1              IF PRODUCT ERROR                             
         JNE   PPRD30              RETURN ERROR                                 
         MVI   ERRFLD,D#PAPRD                                                   
         J     ERR00015                                                         
                                                                                
PPRD30   MVI   ERRFLD,D#PAPRL      IF PRODUCT LENGTH ERROR                      
         J     ERR00068            RETURN ERROR                                 
                                                                                
***********************************************************************         
*        ROUTINE VALIDATES PARTNER PRODUCT-LENGTH                     *         
*        ON ENTRY ... R3 = A(IOKEY)                                   *         
***********************************************************************         
                                                                                
VALPPR   NTR1                                                                   
         OC    RQPAPPR,RQPAPPR     IF PRODUCT PARTNER IS PROVIDED               
         JZ    YES                                                              
         OC    RQPAPPL,RQPAPPL     AND PRODUCT PARTNER LENGTH IS NOT            
         JNZ   PPPR10                                                           
         MVI   RQPAPRL,30          DEFAULT TO 30                                
                                                                                
PPPR10   GOTOR (#VALPRD,AVALPRD),DMCB,RQPAPPR,RQPAPPL                           
         JNE   PPPR20                                                           
         MVC   QPRODPX,QPRODX      VALIDATE PARTNER PRODUCT AND LENGTH          
         J     YES                                                              
                                                                                
PPPR20   CLI   DMCB,1              IF PRODUCT ERROR                             
         JNE   PPPR30              RETURN ERROR                                 
         MVI   ERRFLD,D#PAPPR                                                   
         J     ERR00015                                                         
                                                                                
PPPR30   MVI   ERRFLD,D#PAPPL      IF PRODUCT LENGTH ERROR                      
         J     ERR00068            RETURN ERROR                                 
                                                                                
***********************************************************************         
*        ROUTINE VALIDATES PERCENTAGES                                *         
***********************************************************************         
                                                                                
VALPCTS  NTR1                                                                   
         ZICM  R0,PCTIND,1         IF PERCENTAGES ARE PROVIDED                  
         JZ    YES                                                              
         ZICM  R1,PCTS,3                                                        
VPCTS10  CLC   0(3,R1),=XL3'100'   ASSERT THAT NONE EXCEED 100                  
         JH    ERR00002                                                         
         LA    R1,3(R1)                                                         
         JCT   R0,VPCTS10                                                       
         J     YES                                                              
                                                                                
***********************************************************************         
*        ROUTINE VALIDATES UPDATING APPLICATION FIELD                 *         
***********************************************************************         
                                                                                
VALUAP   NTR1                                                                   
         CLI   RQPAUAP,C'O'        UPDATING APPLICATION MUST BE                 
         JE    YES                 OPTICA                                       
         MVI   ERRFLD,D#PAUAP                                                   
         J     ERR00002                                                         
                                                                                
***********************************************************************         
*        ROUTINE VALIDATES REFERENCE NUMBER AND ENSURES PATTERN       *         
*        EXISTS                                                       *         
*        ON ENTRY ... R3 = A(IOKEY)                                   *         
***********************************************************************         
                                                                                
VALREF   NTR1  BASE=*,LABEL=*                                                   
         USING NPTXKEY,R3                                                       
         BRAS  RE,INIPATKY         INITIALIZE PATTERN KEY                       
                                                                                
         MVC   NPTXR3F,RQPAREF     ENSURE PATTERN EXISTS                        
         XC    NPTXR3F,=3X'FF'                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOXSPDIR+IO3'                            
         CLC   NPTXKEY,IOKEYSAV                                                 
         JNE   ERR00053                                                         
         MVC   SVTXKEY,NPTXKEY                                                  
         DROP  R3                                                               
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOXSPFIL+IO3'                        
         MVI   DATADISP+1,42                                                    
         J     YES                                                              
                                                                                
***********************************************************************         
*        ROUTINE INITIALIZES PATTERN KEY                              *         
*        ON ENTRY ... R3 = A(IOKEY)                                   *         
***********************************************************************         
                                                                                
         USING NPTXKEY,R3                                                       
INIPATKY NTR1  BASE=*,LABEL=*                                                   
         XC    NPTXKEY,NPTXKEY                                                  
         MVC   NPTXKID,=X'0A61'                                                 
         MVC   NPTXAM,QMEDX        BUILD PATTTERN KEY WITH MEDIA                
         MVC   NPTXCLT,QCLTX       AND CLIENT                                   
                                                                                
         CLI   NETIND,0                                                         
         JE    IPK10                                                            
         MVI   NPTXNET,C'$'        NETWORK LIST                                 
         J     IPK30                                                            
IPK10    CLI   RQPANMD,0           OR NETWORK MEDIA                             
         JE    IPK20                                                            
         MVC   NPTXNET+1(1),RQPANMD                                             
         MVI   NPTXNET+2,X'FF'                                                  
         J     IPK30                                                            
IPK20    MVC   NPTXNET,RQPANET     OR NETWORK                                   
                                                                                
IPK30    MVC   NPTXPROG,RQPAPRO    PROGRAM                                      
         OC    RQPADPT,RQPADPT                                                  
         JNZ   IPK40                                                            
         OC    RQPAFED,RQPAFED                                                  
         JZ    IPK50                                                            
IPK40    MVI   NPTXPROG,X'FF'      OR DAYPART-FEED                              
         MVC   NPTXPROG+1(1),QDPT                                               
         MVC   NPTXPROG+2(4),RQPAFED                                            
                                                                                
IPK50    MVC   NPTXPRD,RQPAPRD     PRODUCT                                      
         MVC   NPTXSLN,RQPAPRL     PRODUCT LENGTH                               
         MVC   NPTXPRD2,RQPAPPR    PARTNER PRODUCT                              
         MVC   NPTXSLN2,RQPAPPL    AND PARTNER PRODUCT LENGTH                   
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE VALIDATES NETWORK/NETWORK LIST                       *         
*        ON ENTRY ... R3 = A(IOKEY)                                   *         
***********************************************************************         
                                                                                
VALNETS  NTR1  BASE=*,LABEL=*                                                   
         CLI   NETIND,0            IF NETWORK IS PROVIDED, ENSURE THAT          
         JNE   VNETS10             IT DOES NOT OVERLAP ANOTHER PATTERN          
         MVI   ERRFLD,D#PANET                                                   
         GOTOR CHKOVLP,DMCB,SVTXKEY+NPTXNET-NPTXKEY                             
         JNE   NO                                                               
                                                                                
***********************************************************************         
                                                                                
VNETS10  ZICM  R2,NETIND,1         IF NETWORK LIST IS PROVIDED                  
         JZ    YES                                                              
         ZICM  R4,NETS,3           READ THROUGH EACH NETWORK                    
         MVI   ERRFLD,D#PANETS                                                  
VNETS20  GOTOR READNET,DMCB,0(R4)                                               
         JNE   NO                  ENSURING EACH NETWORK EXISTS                 
         GOTOR CHKOVLP,DMCB,0(R4)  AND DOES NOT OVERLAP ANOTHER                 
         JNE   NO                  PATTERN                                      
         LA    R4,4(R4)                                                         
         JCT   R2,VNETS20                                                       
         J     YES                                                              
                                                                                
***********************************************************************         
*        ROUTINE CHECKS FOR ANY OTHER PATTERNS FOR THIS CLT/PRD       *         
*        WITH SAME MARKET/STATION LIST AND OVERLAPPING DATES          *         
*        ON ENTRY ... R3 = A(IOKEY)                                   *         
*                     AIO3 = A(PATTERN RECORD)                        *         
*                     P1 = A(NETWORK)                                 *         
***********************************************************************         
                                                                                
CHKOVLP  NTR1  BASE=*,LABEL=*                                                   
         L     R1,0(R1)                                                         
         MVC   OVNET,0(R1)         OVNET=NETWORK TO CHECK FOR OVERLAPS          
                                                                                
***********************************************************************         
                                                                                
         USING NPTXKEY,R3                                                       
         MVC   NPTXKEY,SVTXKEY     READ ALL PATTERNS FOR THIS NETWORK           
         MVC   NPTXNET,OVNET                                                    
         MVC   NPTXR3F,=X'A00000'                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOXSPDIR+IO4'                            
         J     COVLP20                                                          
COVLP10  GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IOXSPDIR+IO4'                            
COVLP20  CLC   NPTXKEY(NPTXR3F-NPTXKEY),IOKEYSAV                                
         JNE   YES                                                              
         MVC   FULL(3),RQPAREF                                                  
         XC    FULL(3),=3X'FF'                                                  
         CLC   NPTXR3F,FULL        (SKIPPING PATTERN BEING UPDATED)             
         JE    COVLP10                                                          
         DROP  R3                                                               
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOXSPFIL+IO4'                           
         MVI   DATADISP+1,42                                                    
                                                                                
         USING NPTXKEY,R4                                                       
         L     R4,AIO4                                                          
         CLI   NPTXNET,C'$'        IF PATTERN HAS A NETWORK LIST                
         JNE   COVLP40                                                          
         MVI   ELCODE,X'5B'                                                     
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
COVLP30  BRAS  RE,NEXTEL                                                        
         JNE   COVLP40                                                          
         CLC   OVNET,2(R4)                                                      
         JNE   COVLP30                                                          
         TM    6(R4),X'80'         IF OUR NETWORK IS DELETED                    
         JO    COVLP10             SKIP THIS PATTERN RECORD                     
         DROP  R4                                                               
                                                                                
         USING NPTDATA,R4                                                       
COVLP40  L     R4,AIO4                                                          
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         TM    NPTSTAT,X'80'       IF PATTERN IS DELETED                        
         JO    COVLP10             SKIP THIS PATTERN RECORD                     
                                                                                
         CLC   RQPAPST,NPTEND      IF OUR START DATE AFTER PATTERN              
         JH    COVLP10             END DATE                                     
         CLC   RQPAPEN,NPTSTART    OR OUR END DATE IS BEFORE PATTERN            
         JL    COVLP10             START DATE, WE DO NOT HAVE OVERLAP           
                                                                                
         TM    NPTSTAT,NPTS_TIME   IF PATTERN DOES NOT HAVE TIME                
         JZ    ERR00623            WE HAVE AN OVERLAP                           
         OC    NPTSTIM(4),NPTSTIM                                               
         JZ    ERR00623                                                         
                                                                                
         GOTOR ADJTIM,DMCB,NPTSTIM,NPTSTIMA                                     
         GOTOR ADJTIM,DMCB,NPTETIM,NPTETIMA                                     
                                                                                
         CLC   RQPAPST,NPTEND      IF OUR START DATE MATCHES PATTERN            
         JNE   COVLP50             END DATE                                     
         CLC   RQPASTMA,NPTETIMA   OUR START TIME MUST BE LATER THAN            
         JH    COVLP10             PATTERN END TIME                             
         CLC   NPTSTART,NPTEND     UNLESS PATTERN IS ONLY 1 DAY                 
         JNE   ERR00623                                                         
         CLC   RQPAETMA,NPTSTIMA   THEN IT'S OK IF OUR END TIME IS              
         JNL   ERR00623            LATER THAN PATTERN START TIME                
         J     COVLP10                                                          
                                                                                
COVLP50  CLC   RQPAPEN,NPTSTART    IF OUR END DATE MATCHES THE PATTERN          
         JNE   ERR00623            START DATE                                   
         CLC   RQPAETMA,NPTSTIMA   OUR END TIME MUST BE EARLIER THAN            
         JL    COVLP10             PATTERN START TIME                           
         CLC   NPTSTART,NPTEND     UNLESS PATTERN IS ONLY 1 DAY                 
         JNE   ERR00623                                                         
         CLC   RQPASTMA,NPTETIMA   THEN IT'S OK IF OUR START TIME IS            
         JNL   ERR00623            IS EARLIER THAN PATTERN END TIME             
         J     COVLP10                                                          
                                                                                
***********************************************************************         
                                                                                
         USING NPTXKEY,R3                                                       
ERR00623 LHI   RE,623              PATTERN REF &1 DATES &2-&3 OVERLAP           
                                                                                
         MVC   ERRTXT,SPACES                                                    
         MVC   ERRTXT(L'ETXT623A),ETXT623A                                      
         LA    R2,ERRTXT+L'ETXT623A                                             
                                                                                
         ZICM  R1,NPTXR3F,3                                                     
         X     R1,=X'00FFFFFF'                                                  
         EDIT  (R1),(3,0(R2)),0,ALIGN=LEFT                                      
         AR    R2,R0                                                            
         LA    R2,1(R2)                                                         
                                                                                
         MVC   0(L'ETXT623B,R2),ETXT623B                                        
         LA    R2,L'ETXT623B(R2)                                                
                                                                                
         GOTO1 DATCON,DMCB,(3,NPTSTART),(5,0(R2))                               
         LA    R2,8(R2)                                                         
                                                                                
         MVI   0(R2),C'-'                                                       
         LA    R2,1(R2)                                                         
                                                                                
         CLC   NPTEND,=XL3'FFFFFF'                                              
         JNE   ERR62310                                                         
         MVC   0(3,R2),=CL3'UFN'                                                
         LA    R2,3(R2)                                                         
         J     ERR62320                                                         
                                                                                
ERR62310 GOTO1 DATCON,DMCB,(3,NPTEND),(5,0(R2))                                 
         LA    R2,8(R2)                                                         
                                                                                
ERR62320 MVC   0(L'ETXT623C,R2),ETXT623C                                        
         J     ERR0XIT                                                          
         DROP  R3,R4                                                            
                                                                                
ETXT623A DC    C'Pattern ref '                                                  
ETXT623B DC    C' dates '                                                       
ETXT623C DC    C' overlap'                                                      
                                                                                
***********************************************************************         
*        ROUTINE VALIDATES CHECKSUM                                   *         
*        ON ENTRY ... AIO3 = A(PATTERN RECORD)                        *         
***********************************************************************         
                                                                                
VALCKS   NTR1  BASE=*,LABEL=*                                                   
         CLC   RQPACKS,=X'FFFFFFFF'                                             
         JE    YES                                                              
         L     R4,AIO3                                                          
         GOTOR (#SETCKS,ASETCKS),DMCB,AIO3,L'NPTXKEY(R4),FULL2                  
         CLC   RQPACKS,FULL2                                                    
         JE    YES                                                              
         MVC   RQPACKS,FULL2                                                    
         MVI   ERRFLD,D#PACKS                                                   
         J     ERR00814                                                         
                                                                                
***********************************************************************         
*        ROUTINE ADJUSTS PROVIDED TIME                                *         
*        ON ENTRY ... P1 = A(DATE TO ADJUST)                          *         
*                     P2 = A(ADJUSTED TIME FIELD)                     *         
***********************************************************************         
                                                                                
ADJTIM   NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)            R2 = A(DATE TO ADJUST)                       
         L     R3,4(R1)            R3 = A(ADJUSTED TIME FIELD)                  
                                                                                
         LH    R0,0(R2)                                                         
         CHI   R0,2400                                                          
         BL    *+6                                                              
         XR    R0,R0                                                            
         STH   R0,0(R3)                                                         
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE BUILDS PATTERN RECORD                                *         
***********************************************************************         
                                                                                
BLDREC   NTR1  BASE=*,LABEL=*                                                   
         USING NPTDATA,R4                                                       
         LA    R4,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   NPTDATA,X'10'                                                    
         MVI   NPTDTALN,NPTDTAX-NPTDATA                                         
         OI    NPTSTAT,NPTS_TIME   SET FLAG FOR TIME FIELD IN ELEM              
         TM    PROSTAT,PSADID                                                   
         JZ    BR10                                                             
         OI    NPTSTAT,NPTS_ADID   SET AD-ID FLAG                               
BR10     MVC   NPTDESC,RQPADSC     SAVE DESCRIPTION                             
         MVC   NPTSTART,RQPAPST    PERIOD START DATE                            
         MVC   NPTEND,RQPAPEN      PERIOD END DATE                              
         MVC   NPTSTIM,RQPASTMX    START TIME                                   
         MVC   NPTETIM,RQPAETMX    END TIME                                     
         MVC   NPTDPART,QDPT2      DAYPART CODE                                 
         MVC   NPTS3QNO,SEQNUM     SEQUENCE NUMBER                              
         GOTO1 VHELLO,DMCB,(C'P',=C'XSPFIL'),AIO3,(R4),0                        
         DROP  R4                                                               
                                                                                
***********************************************************************         
                                                                                
         ZICM  R2,NETIND,1         R2=# OF NETWORKS                             
         JZ    BR70                                                             
         ZICM  R3,NETS,3           R3=A(NETWORK INDEX)                          
                                                                                
BR20     L     R4,AIO3             READ THROUGH EXISTING NETWORK                
         MVI   ELCODE,X'5B'        ELEMENTS                                     
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
BR30     BRAS  RE,NEXTEL                                                        
         JNE   BR40                                                             
         CLC   0(4,R3),2(R4)       IF NETWORK ELEMENT ALREADY EXISTS            
         JNE   BR30                                                             
         NI    6(R4),X'7F'         MARK IT UNDELETED                            
         J     BR50                                                             
                                                                                
BR40     XC    ELEM,ELEM           IF NETWORK ELEMENT DOES NOT                  
         MVI   ELEM,X'5B'          ALREADY EXIST                                
         MVI   ELEM+1,7            ADD IT                                       
         MVC   ELEM+2(4),0(R3)                                                  
         GOTO1 VHELLO,DMCB,(C'P',=C'XSPFIL'),AIO3,ELEM,0                        
                                                                                
BR50     LA    R3,4(R3)                                                         
         JCT   R2,BR20                                                          
                                                                                
         XR    R0,R0                                                            
         L     R4,AIO3                                                          
         BRAS  RE,GETEL            ENSURE THE NUMBER OF NETWORK                 
         J     *+8                 ELEMENTS DOES NOT EXCEED 60                  
BR60     BRAS  RE,NEXTEL                                                        
         JNE   BR70                                                             
         CHI   R0,60                                                            
         JE    ERR00694                                                         
         AHI   R0,1                                                             
         J     BR60                                                             
                                                                                
***********************************************************************         
                                                                                
         USING NPTCMLEL,R4                                                      
BR70     LA    R4,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   NPTCMLEL,X'30'                                                   
                                                                                
         ZIC   RE,COMIND                                                        
         MHI   RE,16                                                            
         AHI   RE,2                                                             
         STC   RE,NPTCMLLN                                                      
                                                                                
         LA    R4,NPTCML                                                        
         DROP  R4                                                               
                                                                                
         USING COMTABD,R2                                                       
         L     R2,ACOMTAB                                                       
                                                                                
BR80     CLC   CTCOM,=CL8'*'                                                    
         JNE   BR85                                                             
         MVI   0(R4),C'*'                                                       
         J     BR86                                                             
BR85     MVC   0(8,R4),CTCOM                                                    
BR86     MVC   8(8,R4),CTPIG                                                    
                                                                                
         LA    R4,16(R4)                                                        
         LA    R2,CTLNQ(R2)                                                     
         CLI   0(R2),X'FF'                                                      
         JNE   BR80                                                             
         DROP  R2                                                               
                                                                                
         GOTO1 VHELLO,DMCB,(C'P',=C'XSPFIL'),AIO3,ELEM,0                        
                                                                                
***********************************************************************         
                                                                                
         CLI   PCTIND,1            IF ONE PERCENTAGE PROVIDED                   
         JNE   BR90                                                             
         LA    R4,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   0(R4),X'34'                                                      
         MVI   1(R4),5                                                          
         MVI   2(R4),C'A'                                                       
         MVI   4(R4),100                                                        
         GOTO1 VHELLO,DMCB,(C'P',=C'XSPFIL'),AIO3,(R4),0                        
                                                                                
***********************************************************************         
                                                                                
BR90     CLI   PCTIND,1            IF MORE THAN ONE PERCENTAGE                  
         JNH   BR100               IS PROVIDED                                  
         GOTO1 BLDPCTEL,DMCB,(X'34',0)                                          
                                                                                
***********************************************************************         
                                                                                
BR100    CLI   ROTIND,0            IF ROTATION IS PROVIDED                      
         JE    BR150                                                            
                                                                                
         USING NPTPTNEL,R4                                                      
         LA    R4,ELEM             INITIALIZE ROTATION ELEMENT                  
         XC    ELEM,ELEM                                                        
         MVI   NPTPTNEL,X'32'                                                   
         LHI   R0,2                                                             
         LA    R6,NPTPTN                                                        
                                                                                
         ZIC   RE,ROTIND           FOR EACH ROTATION LETTER                     
         LA    RF,RQPAROT                                                       
                                                                                
         USING COMTABD,R2                                                       
BR110    L     R2,ACOMTAB          CHECK TO SEE IF COMMERCIAL                   
BR120    CLC   CTLET,0(RF)         IS BEING DELETED                             
         JE    BR130                                                            
         LA    R2,CTLNQ(R2)                                                     
         J     BR120                                                            
BR130    CLC   CTCOM,=CL8'*'       IF IT IS BEING DELETED                       
         JE    BR140               REMOVE IT FROM ROTATION                      
         MVC   0(1,R6),0(RF)                                                    
         LA    R6,1(R6)                                                         
         AHI   R0,1                                                             
BR140    LA    RF,1(RF)                                                         
         BCT   RE,BR110                                                         
         DROP  R2                                                               
                                                                                
         STC   R0,NPTPTNLN                                                      
         GOTO1 VHELLO,DMCB,(C'P',=C'XSPFIL'),AIO3,(R4),0                        
         J     BR160                                                            
         DROP  R4                                                               
                                                                                
***********************************************************************         
                                                                                
         USING NPTPTNEL,R4                                                      
BR150    LA    R4,ELEM             IF ROTATION IS NOT PROVIDED                  
         XC    ELEM,ELEM           INITIALIZE COMMERCIAL PATTERN                
         MVI   ELEM,X'32'          ELEMENT                                      
         ZIC   RE,DROTLEN                                                       
         AHI   RE,2                                                             
         STC   RE,NPTPTNLN                                                      
         ZIC   RE,DROTLEN                                                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   NPTPTN(0),DROT                                                   
         DROP  R4                                                               
                                                                                
         GOTO1 VHELLO,DMCB,(C'P',=C'XSPFIL'),AIO3,ELEM,0                        
                                                                                
***********************************************************************         
                                                                                
         TM    PROSTAT,PSPCTADJ    IF PERCENTAGES WERE ADJUSTED                 
         JZ    BR160                                                            
         GOTO1 BLDPCTEL,DMCB,(X'36',0)                                          
                                                                                
***********************************************************************         
                                                                                
BR160    ZICM  R2,CMTIND,1         IF COMMENTS ARE PROVIDED                     
         JZ    BR180               ADD THEM TO RECORD                           
         LHI   R0,1                                                             
         ZICM  R3,CMTS,3                                                        
                                                                                
         USING NPTCMTEL,R4                                                      
         LA    R4,ELEM                                                          
BR170    XC    ELEM,ELEM                                                        
         MVI   NPTCMTEL,X'40'                                                   
         STC   R0,NPTCMTNO                                                      
         MVC   NPTCMT(52),0(R3)                                                 
         GOTOR SETLEN,DMCB,NPTCMTLN,ELEM,L'ELEM                                 
         GOTO1 VHELLO,DMCB,(C'P',=C'XSPFIL'),AIO3,ELEM,0                        
         DROP  R4                                                               
                                                                                
         LA    R3,52(R3)                                                        
         AHI   R0,1                                                             
         JCT   R2,BR170                                                         
                                                                                
***********************************************************************         
                                                                                
         USING HIATABD,R2                                                       
BR180    L     R2,AHIATAB                                                       
BR190    CLI   0(R2),X'FF'                                                      
         JE    YES                                                              
                                                                                
         USING NPTHIAEL,R4                                                      
         LA    R4,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   NPTHIAEL,X'5C'                                                   
         MVI   NPTHIALN,NPTHIAX-NPTHIAEL                                        
         MVC   NPTHIASE,HTSEQ                                                   
                                                                                
         OC    HTDAY,HTDAY                                                      
         JZ    BR200                                                            
         MVC   NPTHIADT+2(1),HTDAYX                                             
                                                                                
BR200    OC    HTDAT,HTDAT                                                      
         JZ    BR210                                                            
         MVC   NPTHIADT,HTDAT                                                   
                                                                                
BR210    MVC   NPTHIATM,HTSTMX                                                  
         MVC   NPTHIATM+2(2),HTETMX                                             
         GOTO1 VHELLO,DMCB,(C'P',=C'XSPFIL'),AIO3,ELEM,0                        
         DROP  R4                                                               
                                                                                
         LA    R2,HTLNQ(R2)                                                     
         J     BR190                                                            
                                                                                
***********************************************************************         
*        ROUTINE BUILDS PERCENTAGE ELEMENT                            *         
*        ON ENTRY ... P1 BYTE 0 = ELEMENT CODE                        *         
***********************************************************************         
                                                                                
BLDPCTEL NTR1                                                                   
         USING NPTPCTEL,R4                                                      
         LA    R4,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVC   ELEM,0(R1)                                                       
         ZIC   RF,PCTIND                                                        
         MHI   RF,3                                                             
         AHI   RF,2                                                             
         STC   RF,NPTPCTLN                                                      
         LA    R4,NPTPCTLT                                                      
         DROP  R4                                                               
                                                                                
         USING COMTABD,R2                                                       
         L     R2,ACOMTAB                                                       
         ZIC   RF,PCTIND                                                        
BPE10    MVC   0(L'NPTPCTLT,R4),CTLET                                           
         MVC   1(L'NPTPCTPC,R4),CTPCT+1                                         
         CLI   ELEM,X'36'                                                       
         JNE   BPE20                                                            
         MVC   1(L'NPTPCTPC,R4),CTOPCT+1                                        
BPE20    LA    R2,CTLNQ(R2)                                                     
         LA    R4,3(R4)                                                         
         JCT   RF,BPE10                                                         
         DROP  R2                                                               
                                                                                
         GOTO1 VHELLO,DMCB,(C'P',=C'XSPFIL'),AIO3,ELEM,0                        
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE BUILDS ACTIVITY ELEMENT FOR ADD                      *         
*        ON ENTRY ... P1 = A(I/O AREA)                                *         
***********************************************************************         
                                                                                
ADDACT   NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)            R2=A(I/O AREA)                               
                                                                                
         USING ACTVD,R4                                                         
         LA    R4,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   ACTVEL,X'F1'                                                     
         MVI   ACTVLEN,20                                                       
         GOTOR DATCON,DMCB,(5,0),(3,FULL)                                       
         MVC   ACTVADDT,FULL                                                    
         MVC   ACTVCHDT,FULL                                                    
         GOTOR BLDACT,DMCB,ACTVADID                                             
         DROP  R4                                                               
                                                                                
         GOTO1 VHELLO,DMCB,(C'P',=C'XSPFIL'),(R2),ELEM,0                        
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE UPDATES ACTVITY ELEMENT                              *         
*        ON ENTRY ... P1 = A(RECORD WITH ACTIVITY ELEMENT)            *         
***********************************************************************         
                                                                                
UPDACT   NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)            R2 = A(I/O AREA)                             
                                                                                
         USING ACTVD,R4                                                         
         LR    R4,R2                                                            
         MVI   ELCODE,X'F1'                                                     
         BRAS  RE,GETEL                                                         
         JE    UACT10                                                           
         GOTOR ADDACT,DMCB,(R2)    IF RECORD DOES NOT ALREADY HAVE              
         J     XIT                 ACTIVITY ELEMENT, GO ADD IT                  
                                                                                
UACT10   XC    ELEM,ELEM                                                        
         ZIC   RE,ACTVLEN                                                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   ELEM(0),0(R4)                                                    
         GOTO1 VHELLO,DMCB,(C'D',=C'XSPFIL'),(X'F1',(R2)),0                     
                                                                                
         LA    R4,ELEM                                                          
         GOTOR DATCON,DMCB,(5,0),(3,FULL)                                       
                                                                                
*                                  IF RECORD ALREADY HAS ACTIVITY               
         CLC   ACTVCHDT,FULL       ELEMENT AND THIS IS FIRST CHANGE             
         JNE   UACT20              TODAY                                        
         CLI   ACTVCHNM,0          OR FIRST CHANGE EVER                         
         JNE   UACT30                                                           
UACT20   MVC   ACTVCHDT,FULL       SET CHANGE DATE AS TODAY                     
         AI    ACTVCHNM,1          AND INCREMENT CHANGE NUMBER                  
UACT30   GOTOR BLDACT,DMCB,ACTVCHID                                             
         GOTO1 VHELLO,DMCB,(C'P',=C'XSPFIL'),(R2),ELEM,0                        
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE BUILDS ACTIVITY ELEMENT                              *         
*        ON ENTRY ... P1 = A(ADDED/CHANGED BY USER ID FIELD)          *         
*                     R4 = A(INITIALIZED ACTIVITY ELEMENT)            *         
***********************************************************************         
                                                                                
         USING ACTVD,R4                                                         
BLDACT   NTR1  BASE=*,LABEL=*                                                   
         L     R3,0(R1)            R3=A(ADDED/CHANGED BY USER ID FIELD)         
                                                                                
         USING TWAD,R1                                                          
         L     R1,ATWA                                                          
         MVC   0(L'ACTVADID,R3),TWAUSRID                                        
         DROP  R1                                                               
                                                                                
         USING FACTSD,R1                                                        
         GOTO1 GETFACT,DMCB,0                                                   
         L     R1,0(R1)                                                         
         TM    FATFLAG,X'08'                                                    
         JZ    BACT10                                                           
         MVC   0(L'ACTVADID,R3),FAPASSWD                                        
         OI    2(R3),X'80'                                                      
         DROP  R1                                                               
                                                                                
         USING SECD,R1                                                          
BACT10   L     R1,ALP                                                           
         L     R1,LP_ASECD-LP_D(R1)                                             
         MVC   ACTVSCID,SECPID                                                  
         MVI   ACTVLEN,ACTVLENQ                                                 
         J     XIT                                                              
         DROP  R1,R4                                                            
                                                                                
***********************************************************************         
*        XITS                                                         *         
***********************************************************************         
                                                                                
         GETEL R4,DATADISP,ELCODE                                               
                                                                                
ERR00001 LHI   RE,1                MISSING INPUT FIELD                          
         J     ERR2XIT                                                          
                                                                                
ERR00002 LHI   RE,SE#ININP         INVALID INPUT FIELD                          
         J     ERR2XIT                                                          
                                                                                
ERR00006 LHI   RE,6                INPUT NOT PERMITTED IN THIS FIELD            
         J     ERR2XIT                                                          
                                                                                
ERR00014 MVI   ERRFLD,D#PACLT      INVALID CLIENT                               
         LHI   RE,SE#INCLT                                                      
         J     ERR2XIT                                                          
                                                                                
ERR00015 LHI   RE,15               INVALID PRODUCT                              
         J     ERR2XIT                                                          
                                                                                
ERR00021 MVI   ERRFLD,D#PADPT                                                   
         LHI   RE,21               INVALID DAYPART                              
         J     ERR2XIT                                                          
                                                                                
ERR00053 MVI   ERRFLD,D#PAREF                                                   
         LHI   RE,SE#RNFND         RECORD NOT FOUND                             
         J     ERR2XIT                                                          
                                                                                
ERR00064 LHI   RE,64               INVALID COMMERCIAL CODE                      
         J     ERR5XIT                                                          
                                                                                
ERR00068 LHI   RE,68                                                            
         J     ERR5XIT             INVALID SPOT LENGTH                          
                                                                                
ERR00076 LHI   RE,76               PATT/CML HAVE DIFFERENT SPOT                 
         J     ERR5XIT             LENGTH                                       
                                                                                
ERR00077 LHI   RE,77               PATT START BEFORE CML RELEASE                
         J     ERR5XIT                                                          
                                                                                
ERR00078 LHI   RE,78               COMMERCIAL PAIR DIFF TYPES                   
         J     ERR5XIT                                                          
                                                                                
ERR00088 LHI   RE,88               PRODUCT AND PARTNER OUT OF SEQUENCE          
         J     ERR5XIT                                                          
                                                                                
ERR00089 MVI   ERRFLD,D#PAROT      MISSING/EXTRA ROTATION                       
         LHI   RE,89                                                            
         J     ERR5XIT                                                          
                                                                                
ERR00093 LHI   RE,93               CML RECALL BEFORE PAT END                    
         J     ERR5XIT                                                          
                                                                                
ERR00094 LHI   RE,94               PATTERN PRODUCT(S) NOT IN COMMERCIAL         
         J     ERR5XIT             RECORD                                       
                                                                                
ERR00502 LHI   RE,502              PROGRAM NOT FOUND                            
         J     ERR13XIT                                                         
                                                                                
ERR00503 LHI   RE,503              NETWORK NOT FOUND                            
         J     ERR13XIT                                                         
                                                                                
ERR00535 LHI   RE,535              COMMERCIAL IS DELETED                        
         J     ERR13XIT                                                         
                                                                                
ERR00560 LHI   RE,560              CANNOT ENTER EQUIVALENT PROGRAM              
         J     ERR13XIT            CODE                                         
                                                                                
ERR00590 LHI   RE,590              NETWORK MEDIA DIGITAL ONLY ALLOWED           
         J     ERR13XIT            IF TN2 PROFILE ALLOWS IT                     
                                                                                
ERR00607 MVI   ERRFLD,D#PANMD                                                   
         LHI   RE,607              INVALID NETWORK MEDIA VALUE                  
         J     ERR13XIT                                                         
                                                                                
ERR00621 LHI   RE,621              PIGGYBACK COMMERCIALS ARE THE SAME           
         J     ERR13XIT                                                         
                                                                                
ERR00624 LHI   RE,624              MUST ENTER NETWORK IF PROGRAM, FEED          
         J     ERR13XIT            OR DAYPART IS ENTERED                        
                                                                                
ERR00626 LHI   RE,626              CANNOT HAVE PROGRAM AND DAYPART              
         J     ERR13XIT                                                         
                                                                                
ERR00628 MVI   ERRFLD,D#PAFED                                                   
         LHI   RE,628              FEED NOT FOUND                               
         J     ERR13XIT                                                         
                                                                                
ERR00629 LHI   RE,629              CANNOT HAVE PROGRAM AND FEED                 
         J     ERR13XIT                                                         
                                                                                
ERR00631 LHI   RE,631              PROGRAM IS NOT COVERED FOR THIS              
         J     ERR13XIT            DATE                                         
                                                                                
ERR00632 MVI   ERRFLD,D#PAUFN      UFN DATE NOT VALID - TN1 PROFILE             
         LHI   RE,632                                                           
         J     ERR13XIT                                                         
                                                                                
ERR00638 MVI   ERRFLD,D#PAPCTS     LIST TOO LONG TO CREATE                      
         LHI   RE,638                                                           
         J     ERR13XIT                                                         
                                                                                
ERR00694 LHI   RE,694              MAXIMUM NUMBER OF NETWORKS EXCEEDED          
         J     ERR13XIT                                                         
                                                                                
ERR00696 LHI   RE,696              COMML PIGGYBACK/SOLO CODE MUST               
         J     ERR13XIT            MATCH USAGE                                  
                                                                                
ERR00768 LHI   RE,768              PLEASE ENTER EITHER PERCENTAGES              
         J     ERR13XIT            OR ROTATION                                  
                                                                                
ERR00777 LHI   RE,777              IF CMML HAS TIME, PATTERN CAN'T              
         J     ERR13XIT            HAVE TIME OR DPT                             
                                                                                
ERR00781 LHI   RE,781              CANNOT HAVE TIME FOR DAYPART OR              
         J     ERR13XIT            PROGRAM SPECIFIC DAYPART                     
                                                                                
ERR00785 LHI   RE,785              COMMERCIAL CANNOT BE USED -                  
         J     ERR13XIT            NETWORK SPECIFIC                             
                                                                                
ERR00804 LHI   RE,804              YOU CAN ONLY HAVE ONE ENTRY FOR              
         J     ERR13XIT            DAY(S)                                       
                                                                                
ERR00807 LHI   RE,807              HIATUS DAYS (AND TIMES) OVERLAP              
         J     ERR13XIT                                                         
                                                                                
ERR00808 LHI   RE,808              HIATUS DATES (AND TIMES) OVERLAP             
         J     ERR13XIT                                                         
                                                                                
ERR00810 LHI   RE,810              CAN'T HAVE DAYS AND DATES IN SAME            
         J     ERR13XIT            RECORD                                       
                                                                                
ERR00814 LHI   RE,814              COMMERCIAL HAS CHANGED - CHKSUM              
         J     ERR13XIT            VALIDATION ERROR                             
                                                                                
ERR0XIT  MVI   LIOBMSYS,0                                                       
         J     ERRXIT                                                           
                                                                                
ERR2XIT  MVI   LIOBMSYS,SE#SY002                                                
         J     ERRXIT                                                           
                                                                                
ERR5XIT  MVI   LIOBMSYS,5                                                       
         J     ERRXIT                                                           
                                                                                
ERR13XIT MVI   LIOBMSYS,SE#SY013                                                
         J     ERRXIT                                                           
                                                                                
ERRXIT   STH   RE,ERROR                                                         
         J     NO                                                               
                                                                                
NO       LHI   RE,0                                                             
         J     *+8                                                              
YES      LHI   RE,1                                                             
         CHI   RE,1                                                             
XIT      XIT1                                                                   
         EJECT                                                                  
FACS     DS    0XL(RFACTABL)       ** SYSTEM FACILITIES **                      
         DC    AL1(RFACEOTQ)                                                    
                                                                                
D#UPLERR EQU   255                                                              
                                                                                
GLOBALS  DS    0D                  ** GLOBAL LITERALS **                        
         LTORG ,                                                                
                                                                                
SAVED    DSECT ,                   ** SAVED STORAGE **                          
                                                                                
PROSTAT  DS    X                   PROGRAM STATUS                               
PSADID   EQU   X'80'               VALIDATE COMMERCIAL ID AS AD-ID              
PSPCTADJ EQU   X'40'               PERCENTAGES WERE ADJUSTED                    
PSSIGCHG EQU   X'20'               SIGNIFICANT CHANGE MADE                      
                                                                                
DATADISP DS    H                   DISPLACEMENT TO FIRST ELEMENT                
ELCODE   DS    XL1                 ELEMENT CODE                                 
                                                                                
SVTN1PRO DS    CL16                SAVED TN1 PROFILE                            
SVTN2PRO DS    CL16                SAVED TN2 PROFILE                            
SVTN3PRO DS    CL16                SAVED TN3 PROFILE                            
                                                                                
NETWORK  DS    CL4                 NETWORK CODE                                 
NETMKT   DS    H                   SAVED NETWORK MARKET NUMBER                  
                                                                                
QPROD1X  DS    X                   PRODUCT HEX CODE                             
QPRODPX  DS    X                   PARTNER PRODUCT HEX CODE                     
                                                                                
LASTVER  DS    H                   LATEST VERSION                               
                                                                                
OVNET    DS    CL(L'NPTXNET)       CHKOVLP NETWORK PARAMETER                    
                                                                                
SVTXKEY  DS    XL(L'NPTXKEY)       SAVED PATTERN KEY                            
         ORG   SVTXKEY                                                          
         DS    XL2                                                              
SVPTXAM  DS    XL1                 SAVED AGENCY/MEDIA                           
SVPTXCLT DS    XL2                 SAVED CLIENT                                 
SVPTXNET DS    CL4                 SAVED NETWORK                                
SVPTXPRG DS    CL6                 SAVED PROGRAM                                
SVPTXPRD DS    CL3                 SAVED PRODUCT                                
SVPTXSLN DS    XL1                 SAVED LENGTH                                 
SVPTXPR2 DS    CL3                 SAVED PARTNER                                
SVPTXSL2 DS    XL1                 SAVED PARTNER LENGTH                         
SVPTXOR3 DS    XL3                 SAVED ORIGINAL REFERENCE                     
         DS    XL2                                                              
SVPTXPSS DS    XL1                                                              
                                                                                
SVADDT   DS    XL(L'ACTVADDT)      SAVED DATE ADDED                             
                                                                                
NPTSTIMA DS    XL2                 ADJUSTED PATTERN START TIME                  
NPTETIMA DS    XL2                 ADJUSTED PATTERN END TIME                    
                                                                                
COMSTAT  DS    XL1                 COMMERCIAL STATUS                            
CSP1MTCH EQU   X'80'               COMMERCIAL 1 MATCHES                         
CSP2MTCH EQU   X'40'               COMMERCIAL 2 MATCHES                         
SVCMLRCL DS    XL(L'CMLRCL)        SAVED COMMERCIAL RECALL DATE                 
                                                                                
OSEQNUM  DS    XL(L'NPTS3QNO)      ORIGINAL PATTERN SEQUENCE NUMBER             
SEQNUM   DS    XL(L'NPTS3QNO)      PATTERN SEQUENCE NUMBER                      
                                                                                
MINPCT   DS    F                   MINIMUM PERCENTAGE                           
MAXPCT   DS    F                   MAXIMUM PERCENTAGE                           
COMDIV   DS    F                   COMMON DIVISOR                               
DROTLEN  DS    X                   DERIVED ROTATION LENGTH                      
DROT     DS    XL60                DERIVED ROTATION                             
                                                                                
ERROR    DS    H                   ERROR NUMBER                                 
ERRFLD   DS    X                   ERROR FIELD                                  
ERRTXT   DS    CL60                ERROR TEXT                                   
LOCALL   EQU   *-SAVED                                                          
                                                                                
ACOMTAB  DS    A                   A(COMMERCIAL TABLE)                          
AHIATAB  DS    A                   A(HIATUS TABLE)                              
                                                                                
RELTAB   DS    XL8                                                              
REMTAB   DS    CL60                                                             
                                                                                
AMASTC   DS    A                   A(MASTC)                                     
RUNMODE  DS    XL(L'RUNPMODE)      DDLINK/RUNNER calling mode                   
                                                                                
SVLPAGY  DS    XL(L'LP_AGY)        SAVED AGENCY ALPHA ID                        
                                                                                
LINKIO   DS    A                   A(LINKIO)                                    
GETPROF  DS    A                   A(GETPROF)                                   
DATAMGR  DS    A                   A(DATAMGR)                                   
DATCON   DS    A                   A(DATCON)                                    
GETFACT  DS    A                   A(GETFACT)                                   
VTRPACK  DS    A                   A(TRPACK)                                    
                                                                                
ALIOB    DS    A                   A(LINKIO control block)                      
                                                                                
LNKVALS  DS    0F                  LNKIO values                                 
LNKSEQ   DS    X                   Sequence #                                   
LNKMAPN  DS    X                   Upload mapcode #                             
LNKMTXT  DS    CL30                Upload mapcode text                          
LNKMSGN  DS    X                   Message #                                    
LNKMSG   DS    CL30                PC reply message                             
LNKVALSL EQU   *-LNKVALS                                                        
                                                                                
***********************************************************************         
*        PATTERN MAINTENANCE REQUEST MAP FIELDS                       *         
***********************************************************************         
                                                                                
RQPAVALS DS    0X                                                               
RQPACLT  DS    CL4                 CLIENT CODE                                  
RQPANMD  DS    CL1                 NETWORK MEDIA                                
RQPANET  DS    CL4                 NETWORK                                      
RQPAPRO  DS    CL6                 PROGRAM                                      
RQPADPT  DS    CL2                 DAYPART CODE                                 
RQPAPRD  DS    CL3                 PRODUCT                                      
RQPAPRL  DS    XL1                 PRODUCT LENGTH                               
RQPAPPR  DS    CL3                 PRODUCT PARTNER                              
RQPAPPL  DS    XL1                 PRODUCT PARTNER LENGTH                       
RQPAREF  DS    XL3                 REFERENCE NUMBER                             
RQPAFED  DS    CL4                 FEED                                         
RQPADSC  DS    CL24                DESCRIPTION                                  
RQPAPST  DS    XL3                 PERIOD START DATE                            
RQPAPEN  DS    XL3                 PERIOD END DATE                              
RQPAUFN  DS    CL1                 UFN DATE?                                    
RQPASTM  DS    CL5                 START TIME                                   
RQPASTMX DS    H                   START TIME CONVERTED                         
RQPASTMA DS    XL2                 START TIME ADJUSTED                          
RQPAETM  DS    CL5                 END TIME                                     
RQPAETMX DS    H                   END TIME CONVERTED                           
RQPAETMA DS    XL2                 END TIME ADJUSTED                            
ROTIND   DS    X                   ROTATION INDEX                               
RQPAROT  DS    CL68                ROTATION                                     
RQPACKS  DS    CL4                 CHECK SUM                                    
RQPAUAP  DS    CL1                 UPDATING APPLICATION                         
RQPAACT  DS    CL1                 ACTION                                       
ACTADD   EQU   C'A'                ADD                                          
ACTCHA   EQU   C'C'                CHANGE                                       
ACTRES   EQU   C'R'                RESTORE                                      
ACTDEL   EQU   C'X'                DELETE                                       
                                                                                
NETIND   DS    X                   NETWORK INDEX                                
NETS     DS    AL3                 A(NETWORKS)                                  
COMINDP  DS    X                   COMMERCIAL POPULATED INDEX                   
COMIND   DS    X                   COMMERCIAL INDEX                             
COMS     DS    AL3                 A(COMMERCIALS)                               
PIGINDP  DS    X                   PIGGYBACK COM POPULATED INDEX                
PIGIND   DS    X                   PIGGYBACK COMMERCIAL INDEX                   
PIGS     DS    AL3                 A(PIGGYBACK COMERCIALS)                      
PCTINDP  DS    X                   PERCENTAGE POPULATED INDEX                   
PCTIND   DS    X                   PERCENTAGE INDEX                             
PCTS     DS    AL3                 A(PERCENTAGES)                               
CMTIND   DS    X                   COMMENT INDEX                                
CMTS     DS    AL3                 A(COMMENTS)                                  
HDYINDP  DS    X                   HIATUS DAYS POPULATED INDEX                  
HDYIND   DS    X                   HIATUS DAYS INDEX                            
HDYS     DS    AL3                 A(HIATUS DAYS)                               
HDTINDP  DS    X                   HIATUS DATES POPULATED INDEX                 
HDTIND   DS    X                   HIATUS DATES INDEX                           
HDTS     DS    AL3                 A(HIATUS DATES)                              
HSTINDP  DS    X                   HIATUS START TIMES POPULATED INDEX           
HSTIND   DS    X                   HIATUS START TIMES INDEX                     
HSTS     DS    AL3                 A(HIATUS START TIMES)                        
HETINDP  DS    X                   HIATUS END TIMES POPULATED INDEX             
HETIND   DS    X                   HIATUS END TIMES INDEX                       
HETS     DS    AL3                 A(HIATUS END TIMES)                          
RQPAVALL EQU   *-RQPAVALS                                                       
         EJECT                                                                  
* Other included books                                                          
         PRINT OFF                                                              
       ++INCLUDE NENAVWORKD                                                     
       ++INCLUDE SPGENESTD                                                      
       ++INCLUDE FALOCKETD                                                      
       ++INCLUDE DDLINKIOD                                                      
       ++INCLUDE DDOFFICED                                                      
       ++INCLUDE SPTRNEQPRG                                                     
       ++INCLUDE SPTRNFEED                                                      
       ++INCLUDE SPTRNPAT                                                       
       ++INCLUDE DDACTIVD                                                       
       ++INCLUDE SPMSGEQUS                                                      
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
**********************************************************************          
*        DSECT FOR COMMERCIAL TABLE                                  *          
**********************************************************************          
                                                                                
COMTABD  DSECT                                                                  
CTLET    DS    CL1                 LETTER/POSITION                              
CTSTAT   DS    XL1                 STATUS                                       
CTIROT   EQU   X'80'               COMMERCIAL REPRESENTED IN ROTATION           
CTCOM    DS    XL8                 COMMERCIAL                                   
CTCOMLEN DS    XL1                 COMMERCIAL LENGTH                            
CTCOMTYP DS    XL1                 COMMERCIAL TYPE                              
CTPIG    DS    XL8                 PIGGYBACK COMMERCIAL                         
CTPIGLEN DS    XL1                 PIGGYBACK LENGTH                             
CTPCT    DS    XL3                 PERCENTAGE (POSSIBLY ADJUSTED)               
CTOPCT   DS    XL3                 ORIGINAL PERCENTAGE                          
CTLNQ    EQU   *-COMTABD                                                        
                                                                                
**********************************************************************          
*        DSECT FOR HIATUS TABLE                                      *          
**********************************************************************          
                                                                                
HIATABD  DSECT                                                                  
HTDAY    DS    CL10                DAY                                          
         ORG   HTDAY                                                            
HTDAYX   DS    CL1                 DAY (CONVERTED)                              
HTSEQ    DS    XL1                 START/END DAY FOR ELEM SEQ                   
         DS    XL8                                                              
HTDAT    DS    CL3                 DATE                                         
HTSTM    DS    CL5                 START TIME                                   
         ORG   HTSTM                                                            
HTSTMX   DS    XL2                 START TIME (CONVERTED)                       
         DS    XL3                                                              
HTETM    DS    CL5                 END TIME                                     
         ORG   HTETM                                                            
HTETMX   DS    XL2                 END TIME (CONVERTED)                         
         DS    XL3                                                              
HTLNQ    EQU   *-HIATABD                                                        
                                                                                
**********************************************************************          
*        MAPCODE EQUATES                                             *          
**********************************************************************          
                                                                                
D#PACLT  EQU   1                   CLIENT                                       
D#PANMD  EQU   2                   NETWORK MEDIA                                
D#PANET  EQU   3                   NETWORK                                      
D#PAPRO  EQU   4                   PROGRAM                                      
D#PADPT  EQU   5                   DAYPART                                      
D#PAPRD  EQU   6                   PRODUCT                                      
D#PAPRL  EQU   7                   PRODUCT LENGTH                               
D#PAPPR  EQU   8                   PARTNER PRODUCT                              
D#PAPPL  EQU   9                   PARTNER PRODUCT LENGTH                       
D#PAREF  EQU   10                  REFERENCE NUMBER                             
D#PAFED  EQU   11                  FEED                                         
D#PADSC  EQU   12                  DESCRIPTION                                  
D#PAPST  EQU   13                  START DATE                                   
D#PAPEN  EQU   14                  END DATE                                     
D#PAUFN  EQU   15                  UFN DATE?                                    
D#PASTM  EQU   16                  START TIME                                   
D#PAETM  EQU   17                  END TIME                                     
D#PANETS EQU   18                  NETWORKS                                     
D#PACOMS EQU   19                  COMMERCIALS                                  
D#PAPIGS EQU   20                  PIGGYBACK COMMERCIALS                        
D#PAPCTS EQU   21                  PERCENTAGES                                  
D#PAROT  EQU   22                  ROTATION                                     
D#PACKS  EQU   23                  CHECK SUM                                    
D#PACMTS EQU   24                  COMMENTS                                     
D#PAHDYS EQU   25                  HIATUS DAYS                                  
D#PAHDTS EQU   26                  HIATUS DATES                                 
D#PAHSTS EQU   27                  HIATUS START TIMES                           
D#PAHETS EQU   28                  HIATUS END TIMES                             
D#PAUAP  EQU   29                  UPDATING APPLICATION                         
D#PAACT  EQU   100                 ACTION                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002NENAV29   03/28/16'                                      
         END                                                                    
