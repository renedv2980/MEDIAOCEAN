*          DATA SET NESFM58    AT LEVEL 002 AS OF 03/31/10                      
*PHASE T31C58A                                                                  
T31C58   TITLE 'NESFM58 - LIMIT RECORDS'                                        
T31C58   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T31C58,R7,RR=R8                                                
         ST    R8,RELO                                                          
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
                                                                                
         OC    VMSUNPK,VMSUNPK                                                  
         JNZ   MAIN02                                                           
         MVC   DMCB+4(4),=X'D9000A1C'                                           
         GOTO1 CALLOV,DMCB,0                                                    
         MVC   VMSUNPK,DMCB         Set A(MSUNPK)                               
                                                                                
MAIN02   OC    DAYVAL,DAYVAL                                                    
         JNZ   MAIN04                                                           
         MVC   DMCB+4(4),=X'D9000A03'                                           
         GOTOR CALLOV,DMCB,0        DAYVAL                                      
         L     RF,DMCB                                                          
         ST    RF,DAYVAL                                                        
                                                                                
MAIN04   OC    TIMVAL,TIMVAL                                                    
         JNZ   MAIN06                                                           
         MVC   DMCB+4(4),=X'D9000A0E'                                           
         GOTOR CALLOV,DMCB,0        TIMVAL                                      
         L     RF,DMCB                                                          
         ST    RF,TIMVAL                                                        
                                                                                
MAIN06   OC    VDAYUNPK,VDAYUNPK                                                
         JNZ   MAIN08                                                           
         MVC   DMCB+4(4),=X'D9000A0F'                                           
         GOTOR CALLOV,DMCB,0        DAYUNPK                                     
         L     RF,DMCB                                                          
         ST    RF,VDAYUNPK                                                      
                                                                                
MAIN08   OC    UNTIME,UNTIME                                                    
         JNZ   MAIN10                                                           
         MVC   DMCB+4(4),=X'D9000A11'                                           
         GOTOR CALLOV,DMCB,0        UNTIME                                      
         L     RF,DMCB                                                          
         ST    RF,UNTIME                                                        
                                                                                
MAIN10   MVI   ACTELOPT,C'N'                                                    
                                                                                
         CLI   MODE,VALKEY         Validate key                                 
         JE    VK                                                               
         CLI   MODE,VALREC         Validate record                              
         JE    VR                                                               
         CLI   MODE,DISPKEY        Display key                                  
         JE    DK                                                               
         CLI   MODE,DISPREC        Display record                               
         JE    DR                                                               
         CLI   MODE,LISTRECS       List record                                  
         JE    LR                                                               
                                                                                
EXIT     XIT1                                                                   
                                                                                
***********************************************************************         
* Validate key                                                        *         
***********************************************************************         
                                                                                
VK       XC    SVVALS(SVVALSL),SVVALS                                           
         CLI   ACTNUM,ACTLIST                                                   
         JE    VKL                                                              
                                                                                
         LA    R2,LIMMEDH           Validate media                              
         CLI   5(R2),0                                                          
         JE    MISSERR                                                          
         GOTOR VALIMED                                                          
         MVC   SVMED,BAGYMD                                                     
                                                                                
         LA    R2,LIMCLIH           Validate client                             
         CLI   5(R2),0                                                          
         JE    MISSERR                                                          
         GOTOR VALICLT                                                          
         MVC   SVCLI,BCLT                                                       
                                                                                
         LA    R2,LIMPRDH           Validate product                            
         CLI   5(R2),0                                                          
         JE    MISSERR                                                          
         GOTOR VALIPRD                                                          
         MVC   SVPRD,QPRD                                                       
                                                                                
         LA    R2,LIMESTH           Validate estimate                           
         CLI   5(R2),0                                                          
         JE    MISSERR                                                          
         MVI   SVEST,0              Default = ALL                               
         CLC   =C'ALL',8(R2)                                                    
         JE    VK02                                                             
         GOTOR VALIEST                                                          
         MVC   SVEST,BEST                                                       
                                                                                
VK02     MVC   SVSTA,=X'FFFFFF'     Default = ALL                               
         LA    R2,LIMNETH           Validate network                            
         CLI   5(R2),0                                                          
         JE    MISSERR                                                          
         CLC   =C'ALL',8(R2)                                                    
         JE    VK04                                                             
         GOTOR VALINTWK                                                         
         MVC   SVSTA,BSTA                                                       
         MVC   SVMKT,BMKT                                                       
                                                                                
VK04     MVC   LKEY,=H'13'                                                      
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'24'                                                  
         MVC   SYSFIL,=C'SPTFILE '                                              
         MVC   SYSDIR,=C'SPTDIR  '                                              
         MVI   USEIO,0                                                          
         XC    FILENAME,FILENAME                                                
                                                                                
         LA    R6,KEY                                                           
         USING PXCRECD,R6                                                       
         XC    KEY,KEY                                                          
         MVI   PXCKTYP,PXCKTYPQ    Set LIMIT key                                
         MVI   PXCKTYP+1,PXCKSUBQ                                               
         MVC   PXCKAGM,SVMED                                                    
         MVC   PXCKCLT,SVCLI                                                    
         MVC   PXCKSTA,SVSTA                                                    
         MVC   PXCKPRD,SVPRD                                                    
         MVC   PXCKEST,SVEST                                                    
         OI    PXCKSTAT,PXCKLIMQ                                                
                                                                                
         MVC   SAVEKEY,KEY                                                      
         MVC   AIO,AIO1                                                         
                                                                                
VKX      J     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* Validate key for LIST                                               *         
***********************************************************************         
                                                                                
VKL      XC    SVVALS(SVVALSL),SVVALS                                           
                                                                                
         LA    R2,LSTMEDH           Validate media                              
         CLI   5(R2),0                                                          
         JE    MISSERR                                                          
         GOTOR VALIMED                                                          
         MVC   SVMED,BAGYMD                                                     
                                                                                
         LA    R2,LSTCLIH           Validate client                             
         CLI   5(R2),0                                                          
         JE    VKL02                                                            
         GOTOR VALICLT                                                          
         MVC   SVCLI,BCLT                                                       
                                                                                
VKL02    LA    R2,LSTPRDH           Validate product                            
         CLI   5(R2),0                                                          
         JE    VKL04                                                            
         GOTOR VALIPRD                                                          
         MVC   SVPRD,QPRD                                                       
                                                                                
VKL04    LA    R2,LSTESTH           Validate estimate                           
         CLI   5(R2),0                                                          
         JE    VKL06                                                            
         MVI   SVEST,0              Default = ALL                               
         CLC   =C'ALL',8(R2)                                                    
         JE    VKL06                                                            
         GOTOR VALIEST                                                          
         MVC   SVEST,BEST                                                       
                                                                                
VKL06    LA    R2,LSTNETH           Validate network                            
         CLI   5(R2),0                                                          
         JE    VKL08                                                            
         MVC   SVSTA,=X'FFFFFF'     Default = ALL                               
         CLC   =C'ALL',8(R2)                                                    
         JE    VKL08                                                            
         GOTOR VALINTWK                                                         
         MVC   SVSTA,BSTA                                                       
         MVC   SVMKT,BMKT                                                       
                                                                                
VKL08    MVC   LKEY,=H'13'                                                      
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'24'                                                  
         MVC   SYSFIL,=C'SPTFILE '                                              
         MVC   SYSDIR,=C'SPTDIR  '                                              
         MVI   USEIO,0                                                          
         XC    FILENAME,FILENAME                                                
                                                                                
         LA    R6,KEY                                                           
         USING PXCRECD,R6                                                       
         XC    KEY,KEY                                                          
         MVI   PXCKTYP,PXCKTYPQ                                                 
         MVI   PXCKTYP+1,PXCKSUBQ                                               
         MVC   PXCKAGM,SVMED                                                    
         MVC   PXCKCLT,SVCLI                                                    
         MVC   PXCKSTA,SVSTA                                                    
         MVC   PXCKPRD,SVPRD                                                    
         MVC   PXCKEST,SVEST                                                    
         OI    PXCKSTAT,PXCKLIMQ                                                
                                                                                
         MVC   SAVEKEY,KEY                                                      
         MVC   AIO,AIO1                                                         
*                                                                               
VKLX     J     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* Validate record                                                     *         
***********************************************************************         
                                                                                
VR       L     R6,AIO                                                           
         USING PXCRECD,R6                                                       
         MVC   PXCKEY,KEY           Build record from scratch                   
         MVC   PXCAGYA,AGENCY                                                   
         MVC   PXCLEN,=X'0018'                                                  
                                                                                
         MVI   PXCEL01,PXCEL01Q     Build activity element                      
         MVI   PXC01LEN,PXCEL01L                                                
         GOTOR DATCON,DMCB,(5,0),(3,PXCACDAT)                                   
                                                                                
         SR    RF,RF                                                            
         ICM   RF,3,PXCLEN                                                      
         AHI   RF,PXCEL01L                                                      
         STCM  RF,3,PXCLEN                                                      
                                                                                
         GOTOR HELLO,DMCB,(C'D',=C'SPTFILE'),(X'06',AIO)                        
         GOTOR HELLO,DMCB,(C'D',=C'SPTFILE'),(X'07',AIO)                        
                                                                                
         MVI   LIMFLG,0                                                         
                                                                                
         LA    R2,LIMLIM1H                                                      
         CLI   5(R2),0                                                          
         JE    MISSERR                                                          
                                                                                
VR02     LA    RF,LIMENDH                                                       
         CR    R2,RF                                                            
         JNL   DR                  Finished validation - redisplay              
         CLC   =C'FF',8(R2)        Test remove this limit                       
         JE    VR22                                                             
         CLI   5(R2),0             Test no limit entered                        
         JE    VR22                                                             
                                                                                
         CLI   8(R2),C'N'          Test net                                     
         JNE   *+14                                                             
         CLC   =C'ALL',LIMNET      Test all networks                            
         JNE   INVNETKY                                                         
                                                                                
         CLC   =C'N+',8(R2)        Test net inclusion                           
         JNE   VR04                                                             
         TM    LIMFLG,LIMNEXCL     Can't have both types                        
         JNZ   BOTHERR                                                          
         OI    LIMFLG,LIMNINCL                                                  
         J     VRNINCL                                                          
                                                                                
VR04     CLC   =C'N-',8(R2)        Test net exclusion                           
         JNE   VR06                                                             
         TM    LIMFLG,LIMNINCL     Can't have both types                        
         JNZ   BOTHERR                                                          
         OI    LIMFLG,LIMNEXCL                                                  
         J     VRNEXCL                                                          
                                                                                
VR06     CLI   9(R2),C'+'          Test inclusion                               
         JE    INVINCER                                                         
         CLC   =C'D-',8(R2)        Test day exclusion                           
         JE    VRDEXCL                                                          
         CLC   =C'T-',8(R2)        Test time exclusion                          
         JE    VRTEXCL                                                          
         CLC   =C'P-',8(R2)        Test program exclusion                       
         JE    VRPEXCL                                                          
         CLC   =C'Y-',8(R2)        Test day/time exclusion                      
         JE    VRYEXCL                                                          
         J     INVERR                                                           
                                                                                
VRNINCL  XC    ELEM,ELEM           Build net inclusion element                  
E        USING L06ELD,ELEM                                                      
         MVI   E.L06ELID,L06ELIDQ                                               
         MVI   E.L06ELLN,L06ELL                                                 
         MVI   E.L06STAT,L06STNET                                               
         CLI   5(R2),6             Test length no more than 4 + (N+)            
         JH    INVERR                                                           
         CLI   5(R2),5                                                          
         JE    VRNI02                                                           
         CLI   13(R2),C'A'         And must be alphabetic                       
         JL    INVERR                                                           
         CLI   13(R2),C'Z'                                                      
         JH    INVERR                                                           
VRNI02   MVC   E.L06LIM,10(R2)     Set net name                                 
         OC    E.L06LIM,SPACES                                                  
                                                                                
         BAS   RE,GETDATE                                                       
         MVC   E.L06STDTE,FULL     Set start date                               
         MVC   E.L06ENDTE,FULL+2   Set end date                                 
         MVI   BYTE,L06ELIDQ       Set elem code for HELLO                      
         J     VR20                                                             
         DROP  E                                                                
                                                                                
VRNEXCL  XC    ELEM,ELEM           Build net exclusion element                  
E        USING L07ELD,ELEM                                                      
         MVI   E.L07ELID,L07ELIDQ                                               
         MVI   E.L07ELLN,L07ELL                                                 
         MVI   E.L07STAT,L07STNET                                               
         CLI   5(R2),6             Test length no more than 4 + (N-)            
         JH    INVERR                                                           
         CLI   5(R2),5                                                          
         JE    VRNE02                                                           
         CLI   13(R2),C'A'         And must be alphabetic                       
         JL    INVERR                                                           
         CLI   13(R2),C'Z'                                                      
         JH    INVERR                                                           
VRNE02   MVC   E.L07LIM,10(R2)     Set net                                      
         OC    E.L07LIM,SPACES                                                  
                                                                                
         BAS   RE,GETDATE                                                       
         MVC   E.L07STDTE,FULL     Set start date                               
         MVC   E.L07ENDTE,FULL+2   Set end date                                 
         MVI   BYTE,L07ELIDQ       Set elem code for HELLO                      
         J     VR20                                                             
         DROP  E                                                                
                                                                                
VRDEXCL  XC    ELEM,ELEM           Build day exclusion element                  
E        USING L07ELD,ELEM                                                      
         MVI   E.L07ELID,L07ELIDQ                                               
         MVI   E.L07ELLN,L07ELL                                                 
         MVI   E.L07STAT,L07STDAY                                               
                                                                                
         BAS   RE,VALDAY           Validate day                                 
         MVC   E.L07LIM(1),WORK    Set day                                      
                                                                                
         BAS   RE,GETDATE                                                       
         MVC   E.L07STDTE,FULL     Set start date                               
         MVC   E.L07ENDTE,FULL+2   Set end date                                 
         MVI   BYTE,L07ELIDQ       Set elem code for HELLO                      
         J     VR20                                                             
         DROP  E                                                                
                                                                                
VRTEXCL  XC    ELEM,ELEM           Build time exclusion element                 
E        USING L07ELD,ELEM                                                      
         MVI   E.L07ELID,L07ELIDQ                                               
         MVI   E.L07ELLN,L07ELL                                                 
         MVI   E.L07STAT,L07STTIM                                               
                                                                                
         BAS   RE,VALTIME          Validate time                                
         MVC   E.L07LIM(4),WORK    Set time                                     
                                                                                
         BAS   RE,GETDATE                                                       
         MVC   E.L07STDTE,FULL     Set start date                               
         MVC   E.L07ENDTE,FULL+2   Set end date                                 
         MVI   BYTE,L07ELIDQ       Set elem code for HELLO                      
         J     VR20                                                             
         DROP  E                                                                
                                                                                
VRPEXCL  CLC   =C'ALL',LIMNET      Test network all                             
         JE    NETERR                                                           
         CLI   5(R2),18            Test max program character length            
         JH    INVPGMLN                                                         
                                                                                
         XC    ELEM,ELEM           Build program exclusion element              
E        USING L07ELD,ELEM                                                      
         MVI   E.L07ELID,L07ELIDQ                                               
         MVI   E.L07ELLN,L07ELL                                                 
         MVI   E.L07STAT,L07STPRG                                               
         MVC   E.L07LIM,10(R2)     Set program name                             
         OC    E.L07LIM,SPACES                                                  
                                                                                
         BAS   RE,GETDATE                                                       
         MVC   E.L07STDTE,FULL     Set start date                               
         MVC   E.L07ENDTE,FULL+2   Set end date                                 
         MVI   BYTE,L07ELIDQ       Set elem code for HELLO                      
         J     VR20                                                             
         DROP  E                                                                
                                                                                
VRYEXCL  XC    ELEM,ELEM           Build day/time exclusion element             
E        USING L07ELD,ELEM                                                      
         MVI   E.L07ELID,L07ELIDQ                                               
         MVI   E.L07ELLN,L07ELL                                                 
         MVI   E.L07STAT,L07STDTM                                               
                                                                                
         XC    TEMPFLDH(TEMPFLDL),TEMPFLDH                                      
         MVC   TEMPFLD(2),=C'D-'                                                
         LA    RE,2                Init l'field                                 
         LA    RF,TEMPFLD+2                                                     
         LA    R4,10(R2)                                                        
VRY02    CLI   0(R4),C'/'          Test finished day                            
         JE    VRY04                                                            
         MVC   0(1,RF),0(R4)                                                    
         AHI   RE,1                                                             
         AHI   RF,1                                                             
         AHI   R4,1                                                             
         J     VRY02                                                            
                                                                                
VRY04    STC   RE,TEMPFLDH+5       Set l'input                                  
         AHI   RE,8                                                             
         STC   RE,TEMPFLDH         Set l'field                                  
                                                                                
         ST    R2,FULL             Save address on screen                       
         LA    R2,TEMPFLDH                                                      
         BAS   RE,VALDAY           Validate day                                 
         MVC   E.L07LIM(1),WORK    Set day                                      
                                                                                
         XC    TEMPFLDH(TEMPFLDL),TEMPFLDH                                      
         MVC   TEMPFLD(2),=C'T-'                                                
         LA    RE,2                Init l'field                                 
         LA    RF,TEMPFLD+2                                                     
         AHI   R4,1                                                             
VRY06    CLI   0(R4),C' '          Test finished time                           
         JE    VRY08                                                            
         CLI   0(R4),0                                                          
         JE    VRY08                                                            
         MVC   0(1,RF),0(R4)                                                    
         AHI   RE,1                                                             
         AHI   RF,1                                                             
         AHI   R4,1                                                             
         J     VRY06                                                            
                                                                                
VRY08    STC   RE,TEMPFLDH+5       Set l'input                                  
         AHI   RE,8                                                             
         STC   RE,TEMPFLDH         Set l'field                                  
                                                                                
         LA    R2,TEMPFLDH                                                      
         BAS   RE,VALTIME          Validate time                                
         MVC   E.L07LIM+1(4),WORK  Set time                                     
                                                                                
         L     R2,FULL             Restore screen address                       
         BAS   RE,GETDATE                                                       
         MVC   E.L07STDTE,FULL     Set start date                               
         MVC   E.L07ENDTE,FULL+2   Set end date                                 
         MVI   BYTE,L07ELIDQ       Set elem code for HELLO                      
         J     VR20                                                             
         DROP  E                                                                
                                                                                
VR20     GOTOR HELLO,DMCB,(C'P',=C'SPTFILE'),(BYTE,AIO),ELEM,0                  
                                                                                
VR22     ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         J     VR02                                                             
         DROP  R6                                                               
                                                                                
***********************************************************************         
* Validate day                                                        *         
* Exit - Work = Day                                                   *         
***********************************************************************         
                                                                                
VALDAY   NTR1                                                                   
         ZIC   R3,5(R2)                                                         
         SHI   R3,2                                                             
         GOTOR DAYVAL,DMCB,((R3),10(R2)),WORK,WORK+16                           
         CLI   WORK,0                                                           
         JE    INVERR                                                           
         J     EXIT                                                             
                                                                                
***********************************************************************         
* Validate time                                                       *         
* Exit - Work = Time                                                  *         
***********************************************************************         
                                                                                
VALTIME  NTR1                                                                   
         LA    R3,10(R2)                                                        
VT02     CLI   0(R3),C'/'                                                       
         JE    VT04                                                             
         CLI   0(R3),C' '                                                       
         JNH   VT04                                                             
         AHI   R3,1                                                             
         J     VT02                                                             
                                                                                
VT04     LA    RF,10(R2)                                                        
         SR    R3,RF                                                            
         GOTOR TIMVAL,DMCB,((R3),10(R2)),WORK                                   
         CLI   DMCB,X'FF'                                                       
         JE    INVERR                                                           
         J     EXIT                                                             
                                                                                
***********************************************************************         
* Validate date range                                                 *         
* Exit - Full = Start Date                                            *         
*        Full+2 = End date                                            *         
***********************************************************************         
                                                                                
GETDATE  NTR1                                                                   
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
                                                                                
         XC    WORK,WORK                                                        
         XC    FULL,FULL                                                        
                                                                                
         CLI   SVEST,0             Test all estimates                           
         JE    *+12                                                             
         CLI   5(R2),0                                                          
         JE    EXIT                                                             
                                                                                
         LA    R3,8(R2)                                                         
         GOTOR DATVAL,DMCB,(0,(R3)),WORK                                        
         OC    DMCB(4),DMCB                                                     
         JZ    INVERR                                                           
         A     R3,DMCB                                                          
         CLI   0(R3),C'-'           Test 2nd date                               
         JNE   GD02                                                             
         AHI   R3,1                                                             
         GOTOR DATVAL,DMCB,(0,(R3)),WORK+6                                      
         OC    DMCB(4),DMCB                                                     
         JZ    INVERR                                                           
                                                                                
GD02     GOTOR DATCON,DMCB,(0,WORK),(2,FULL)                                    
         CLI   WORK+6,0                                                         
         JE    EXIT                                                             
         GOTOR DATCON,DMCB,(0,WORK+6),(2,FULL+2)                                
         J     EXIT                                                             
                                                                                
***********************************************************************         
* Display record                                                                
***********************************************************************         
                                                                                
DR       GOTOR CLRSCRN              Clear screen for display                    
                                                                                
         L     R6,AIO                                                           
         LA    R2,LIMLIM1H                                                      
                                                                                
         MVI   ELCODE,L06ELIDQ                                                  
         BAS   RE,GETEL                                                         
         J     *+8                                                              
DR02     BAS   RE,NEXTEL                                                        
         JNE   DR20                                                             
         USING L06ELD,R6                                                        
                                                                                
         LA    RF,LIMENDH          Test end of screen                           
         CR    R2,RF                                                            
         JNL   DRX                                                              
         OI    6(R2),X'80'                                                      
                                                                                
         TM    L06STAT,L06STNET    Test net inclusion                           
         JZ    DR02                                                             
         MVC   8(2,R2),=C'N+'                                                   
         MVC   10(L'L06LIM,R2),L06LIM                                           
                                                                                
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         OI    6(R2),X'80'                                                      
                                                                                
         OC    L06STDTE,L06STDTE                                                
         JZ    DR12                                                             
         GOTOR DATCON,DMCB,(2,L06STDTE),(5,8(R2))                               
         CLI   L06ENDTE,0                                                       
         JE    DR12                                                             
         MVI   16(R2),C'-'                                                      
         GOTOR DATCON,DMCB,(2,L06ENDTE),(5,17(R2))                              
*                                                                               
DR12     ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         J     DR02                                                             
         DROP  R6                                                               
                                                                                
DR20     L     R6,AIO                                                           
                                                                                
         MVI   ELCODE,L07ELIDQ                                                  
         BAS   RE,GETEL                                                         
         J     *+8                                                              
DR22     BAS   RE,NEXTEL                                                        
         JNE   DRX                                                              
         USING L07ELD,R6                                                        
                                                                                
         LA    RF,LIMENDH          Test end of screen                           
         CR    R2,RF                                                            
         JNL   DRX                                                              
         OI    6(R2),X'80'                                                      
                                                                                
         TM    L07STAT,L06STNET    Test net exclusion                           
         JZ    DR24                                                             
         MVC   8(2,R2),=C'N-'                                                   
         MVC   10(L'L07LIM,R2),L07LIM                                           
         J     DR34                                                             
                                                                                
DR24     TM    L07STAT,L07STDAY    Test day exclusion                           
         JZ    DR26                                                             
         MVC   8(2,R2),=C'D-'                                                   
                                                                                
         GOTO1 VDAYUNPK,DMCB,L07LIM,(X'07',10(R2))                              
         J     DR34                                                             
                                                                                
DR26     TM    L07STAT,L07STTIM    Test time exclusion                          
         JZ    DR28                                                             
         MVC   8(2,R2),=C'T-'                                                   
                                                                                
         GOTOR UNTIME,DMCB,L07LIM,10(R2)                                        
         J     DR34                                                             
                                                                                
DR28     TM    L07STAT,L07STPRG    Test program exclusion                       
         JZ    DR30                                                             
         MVC   8(2,R2),=C'P-'                                                   
         MVC   10(L'L07LIM,R2),L07LIM                                           
         J     DR34                                                             
                                                                                
DR30     TM    L07STAT,L07STDTM    Test day/time  exclusion                     
         JZ    DR22                                                             
         MVC   8(2,R2),=C'Y-'                                                   
                                                                                
         GOTO1 VDAYUNPK,DMCB,L07LIM,(X'07',10(R2))                              
         LA    R3,10(R2)                                                        
DR32     CLI   0(R3),0                                                          
         JE    *+12                                                             
         AHI   R3,1                                                             
         J     DR32                                                             
         MVI   0(R3),C'/'                                                       
         GOTOR UNTIME,DMCB,L07LIM+1,1(R3)                                       
                                                                                
DR34     ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         OI    6(R2),X'80'                                                      
                                                                                
         OC    L07STDTE,L07STDTE                                                
         JZ    DR36                                                             
         GOTOR DATCON,DMCB,(2,L07STDTE),(5,8(R2))                               
         CLI   L07ENDTE,0                                                       
         JE    DR36                                                             
         MVI   16(R2),C'-'                                                      
         GOTOR DATCON,DMCB,(2,L07ENDTE),(5,17(R2))                              
*                                                                               
DR36     ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         J     DR22                                                             
                                                                                
DRX      MVC   AIO,AIO1                                                         
         J     EXIT                                                             
         DROP  R6                                                               
                                                                                
***********************************************************************         
* Display key                                                                   
***********************************************************************         
                                                                                
DK       L     R6,AIO                                                           
         USING PXCRECD,R6                                                       
                                                                                
         MVI   LIMMED,C'N'                                                      
         OI    LIMMEDH+6,X'80'                                                  
                                                                                
         GOTO1 CLUNPK,DMCB,(BCLIAAN,PXCKCLT),LIMCLI                             
         OI    LIMCLIH+6,X'80'                                                  
                                                                                
         MVC   LIMPRD,PXCKPRD                                                   
         OI    LIMPRDH+6,X'80'                                                  
                                                                                
         MVC   LIMNET,=C'ALL '                                                  
         OI    LIMNETH+6,X'80'                                                  
         CLC   PXCKSTA,=X'FFFFFF'                                               
         JE    DK02                                                             
         XC    WORK,WORK                                                        
         MVC   WORK+2(L'PXCKSTA),PXCKSTA                                        
         GOTO1 VMSUNPK,DMCB,WORK,WORK+5,WORK+10                                 
         MVC   LIMNET,WORK+10                                                   
                                                                                
DK02     MVC   LIMEST,=C'ALL'                                                   
         OI    LIMESTH+6,X'80'                                                  
         CLI   PXCKEST,0                                                        
         JE    DKX                                                              
         EDIT  PXCKEST,LIMEST,ALIGN=LEFT                                        
                                                                                
DKX      J     EXIT                                                             
         DROP  R6                                                               
                                                                                
***********************************************************************         
* List records                                                                  
***********************************************************************         
                                                                                
LR       GOTOR CHKKEY                                                           
                                                                                
         CLI   KEY,0                First time through?                         
         JNE   *+10                                                             
         MVC   PREVKEY,SAVEKEY                                                  
                                                                                
LR10     XC    KEY,KEY                                                          
         MVC   KEY(L'PXCKEY),PREVKEY                                            
         GOTO1 HIGH                                                             
         J     LR20                                                             
                                                                                
LRSEQ    GOTO1 SEQ                                                              
                                                                                
LR20     CLC   KEY(PXCKCLT-PXCKEY),KEYSAVE                                      
         JNE   LRX                                                              
                                                                                
         LA    R6,KEY                                                           
         USING PXCRECD,R6                                                       
                                                                                
         TM    PXCKSTAT,PXCKLIMQ   Test LIMIT record                            
         JZ    LRSEQ                                                            
                                                                                
         OC    SVCLI,SVCLI                                                      
         JZ    *+14                                                             
         CLC   PXCKCLT,SVCLI                                                    
         JNE   LRSEQ                                                            
                                                                                
         OC    SVSTA,SVSTA                                                      
         JZ    *+14                                                             
         CLC   PXCKSTA,SVSTA                                                    
         JNE   LRSEQ                                                            
                                                                                
         OC    SVPRD,SVPRD                                                      
         JZ    *+14                                                             
         CLC   PXCKPRD,SVPRD                                                    
         JNE   LRSEQ                                                            
                                                                                
         CLC   LSTEST,=C'ALL'                                                   
         JNE   LR22                                                             
         CLI   PXCKEST,0            All?                                        
         JNE   LRSEQ                                                            
         J     LR24                                                             
                                                                                
LR22     CLI   SVEST,0                                                          
         JE    *+14                                                             
         CLC   PXCKEST,SVEST                                                    
         JNE   LRSEQ                                                            
                                                                                
LR24     CLC   LSTNET,=C'ALL'                                                   
         JNE   LR26                                                             
         CLC   PXCKSTA,=X'FFFFFF'   All?                                        
         JNE   LRSEQ                                                            
         J     LR28                                                             
                                                                                
LR26     OC    SVSTA,SVSTA                                                      
         JZ    *+14                                                             
         CLC   PXCKSTA,SVSTA                                                    
         JNE   LRSEQ                                                            
                                                                                
LR28     MVC   PREVKEY,KEY                                                      
                                                                                
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
                                                                                
         LA    R5,LISTAR                                                        
         USING PLINED,R5                                                        
         L     R6,AIO                                                           
         USING PXCRECD,R6                                                       
                                                                                
         GOTOR CLUNPK,DMCB,PXCKCLT,PCLI                                         
         MVC   PPRD,PXCKPRD                                                     
                                                                                
         MVC   PNET,=C'ALL '                                                    
         CLC   PXCKSTA,=X'FFFFFF'                                               
         JE    LR32                                                             
         XC    WORK,WORK                                                        
         MVC   WORK+2(L'PXCKSTA),PXCKSTA                                        
         GOTO1 VMSUNPK,DMCB,WORK,WORK+5,WORK+10                                 
         MVC   PNET,WORK+10                                                     
                                                                                
LR32     MVC   PEST,=C'ALL'                                                     
         CLI   PXCKEST,0                                                        
         JE    LR40                                                             
         EDIT  PXCKEST,PEST,ALIGN=LEFT                                          
                                                                                
LR40     GOTO1 LISTMON                                                          
         J     LRSEQ                                                            
                                                                                
LRX      J     EXIT                                                             
         DROP  R6                                                               
                                                                                
*********************************************************************           
* Check if key fields changed                                                   
*********************************************************************           
                                                                                
CHKKEY   NTR1                                                                   
         TM    LSTMEDH+4,X'80'                                                  
         JO    CHKK10                                                           
         TM    LSTCLIH+4,X'80'                                                  
         JO    CHKK10                                                           
         TM    LSTPRDH+4,X'80'                                                  
         JO    CHKK10                                                           
         TM    LSTESTH+4,X'80'                                                  
         JO    CHKK10                                                           
         TM    LSTNETH+4,X'80'                                                  
         JZ    CHKKEYX                                                          
CHKK10   XC    KEY,KEY                                                          
CHKKEYX  J     EXIT                                                             
                                                                                
***********************************************************************         
* Clear screen for display                                            *         
***********************************************************************         
                                                                                
CLRSCRN  NTR1                                                                   
         LA    R2,LIMLIM1H                                                      
         LA    R3,LIMENDH                                                       
                                                                                
CLRS02   CR    R2,R3                                                            
         JE    CLRSCRNX                                                         
         XC    8(L'LIMLIM1,R2),8(R2)                                            
         OI    6(R2),X'80'                                                      
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         XC    8(L'LIMDAT1,R2),8(R2)                                            
         OI    6(R2),X'80'                                                      
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         J     CLRS02                                                           
                                                                                
CLRSCRNX J     EXIT                                                             
                                                                                
MISSERR  MVI   ERROR,MISSING                                                    
         J     TRAPERR                                                          
                                                                                
INVERR   MVI   ERROR,INVALID                                                    
         J     TRAPERR                                                          
                                                                                
INVPGMLN MVC   ERRNUM,=AL2(INVPGMER)                                            
         J     TRAPERR2                                                         
                                                                                
INVNETKY MVC   ERRNUM,=AL2(INVNETER)                                            
         J     TRAPERR2                                                         
                                                                                
INVINCER MVC   ERRNUM,=AL2(INVINRUL)                                            
         J     TRAPERR2                                                         
                                                                                
BOTHERR  MVC   ERRNUM,=AL2(INVBTHER)                                            
         J     TRAPERR2                                                         
                                                                                
NETERR   MVC   ERRNUM,=AL2(MISNETER)                                            
         J     TRAPERR2                                                         
                                                                                
MSGERR   MVI   ERROR,0                                                          
         GOTO1 ERREX2                                                           
                                                                                
TRAPERR  OC    ERRDISP,ERRDISP                                                  
         JZ    TRAPEND                                                          
         L     RE,SYSPARMS                                                      
         L     RE,0(RE)                                                         
         USING TIOBD,RE                                                         
         OI    TIOBINDS,TIOBSETC                                                
         LR    RF,R2                                                            
         SR    RF,RA                                                            
         STCM  RF,3,TIOBCURD                                                    
         MVC   TIOBCURI,ERRDISP+1                                               
TRAPEND  MVI   ERROPT,0                                                         
         GOTOR ERREX                                                            
                                                                                
TRAPERR2 OI    GENSTAT2,USGETTXT                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTMSGNO,ERRNUM                                                   
         MVI   GTMTYP,GTMERR                                                    
         MVI   GTMSYS,2                                                         
         MVC   AIO,AIO1                                                         
         GOTOR ERREX                                                            
         DROP  RF                                                               
                                                                                
INVNETER EQU   1339                                                             
INVINRUL EQU   1340                                                             
INVPGMER EQU   1341                                                             
INVBTHER EQU   1342                                                             
MISNETER EQU   1343                                                             
                                                                                
         GETEL R6,24,ELCODE                                                     
                                                                                
         DS    0F                                                               
ZEROES   DC    20C'0'                                                           
ERRDISP  DS    H                                                                
RELO     DS    A                                                                
         LTORG                                                                  
         DROP  R7,RB                                                            
                                                                                
       ++INCLUDE NESFMFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE DDGENTWA                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NESFM81D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NESFM80D                                                       
                                                                                
       ++INCLUDE NESFMWORKD                                                     
       ++INCLUDE SPGENPXC                                                       
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE SPDDEQUS                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FATIOB                                                         
       ++INCLUDE FAGETTXTD                                                      
                                                                                
GEND     DSECT                                                                  
SPOOLD   DSECT                                                                  
SYSD     DSECT                                                                  
         ORG   SYSSPARE+220                                                     
                                                                                
SAVEKEY  DS    CL(L'PXCKEY)                                                     
PREVKEY  DS    CL(L'PXCKEY)                                                     
ERRNUM   DS    XL2                                                              
                                                                                
SVVALS   DS    0X                  ** Saved key values **                       
SVMED    DS    XL1                 Media                                        
SVCLI    DS    XL2                 Client                                       
SVSTA    DS    XL3                 Station                                      
SVPRD    DS    CL3                 Product                                      
SVEST    DS    XL1                 Estimate                                     
SVMKT    DS    CL4                 Market                                       
SVVALSL  EQU   *-SVVALS                                                         
                                                                                
LIMFLG   DS    X                   Limit flag                                   
LIMNINCL EQU   X'80'               Net inclusion                                
LIMNEXCL EQU   X'40'               Net exclusion                                
LIMDINCL EQU   X'20'               Day inclusion                                
LIMDEXCL EQU   X'10'               Dat exclusion                                
LIMTINCL EQU   X'08'               Time inclusion                               
LIMTEXCL EQU   X'04'               Time exclusion                               
LIMPINCL EQU   X'02'               Program inclusion                            
LIMPEXCL EQU   X'01'               Program exclusion                            
                                                                                
TEMP     DS    X                                                                
SCANTBL  DS    XL256                                                            
NLINES   DS    X                                                                
FNDX     DS    X                                                                
MYKEY    DS    CL(L'KEY)                                                        
                                                                                
TEMPFLDH DS    XL8                                                              
TEMPFLD  DS    XL10                                                             
TEMPFLDL EQU   *-TEMPFLDH                                                       
                                                                                
VDAYUNPK DS    F                                                                
                                                                                
PLINED   DSECT                                                                  
PCLI     DS    CL3                                                              
         DS    CL2                                                              
PPRD     DS    CL3                                                              
         DS    CL2                                                              
PEST     DS    CL3                                                              
         DS    CL2                                                              
PNET     DS    CL4                                                              
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002NESFM58   03/31/10'                                      
         END                                                                    
