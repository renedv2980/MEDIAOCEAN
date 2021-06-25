*          DATA SET NESFM56    AT LEVEL 002 AS OF 06/04/08                      
*PHASE T31C56A                                                                  
T31C56   TITLE 'NESFM56 - PRODUCT EXCLUSION RECORDS'                            
T31C56   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T31C56,R7,RR=R8                                                
         ST    R8,RELO                                                          
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         MVC   DMCB+4(4),=X'D9000A1C'                                           
         GOTO1 CALLOV,DMCB,0                                                    
         MVC   VMSUNPK,DMCB         A(MSUNPK) to display network                
*                                                                               
         MVI   ACTELOPT,C'N'                                                    
*                                                                               
         CLI   MODE,VALKEY                                                      
         JE    VK                                                               
         CLI   MODE,VALREC                                                      
         JE    VR                                                               
         CLI   MODE,DISPKEY                                                     
         JE    DK                                                               
         CLI   MODE,DISPREC                                                     
         JE    DR                                                               
         CLI   MODE,LISTRECS                                                    
         JE    LR                                                               
*                                                                               
XIT      XIT1                                                                   
********************************************************************            
* Validate key                                                                  
********************************************************************            
VK       XC    SVVALS(SVVALSX-SVVALS),SVVALS                                    
         CLI   ACTNUM,ACTLIST                                                   
         JE    VKL                                                              
*                                                                               
         LA    R2,PXCMEDH           Validate media                              
         CLI   5(R2),0                                                          
         JE    MISSERR                                                          
         GOTOR VALIMED                                                          
         MVC   SVMED,BAGYMD                                                     
*                                                                               
         LA    R2,PXCCLIH           Validate client                             
         CLI   5(R2),0                                                          
         JE    MISSERR                                                          
         GOTOR VALICLT                                                          
         MVC   SVCLI,BCLT                                                       
*                                                                               
         LA    R2,PXCPRDH           Validate product                            
         CLI   5(R2),0                                                          
         JE    MISSERR                                                          
         GOTOR VALIPRD                                                          
         MVC   SVPRD,QPRD                                                       
*                                                                               
         LA    R2,PXCESTH           Validate estimate                           
         CLI   5(R2),0                                                          
         JE    MISSERR                                                          
         MVI   SVEST,0              Default = ALL                               
         CLC   =C'ALL',8(R2)                                                    
         JE    VK02                                                             
         GOTOR VALIEST                                                          
         MVC   SVEST,BEST                                                       
*                                                                               
VK02     LA    R2,PXCNETH           Validate network                            
         CLI   5(R2),0                                                          
         JE    MISSERR                                                          
         MVC   SVSTA,=X'FFFFFF'     Default = ALL                               
         CLC   =C'ALL',8(R2)                                                    
         JE    VK04                                                             
         GOTOR VALINTWK                                                         
         MVC   SVSTA,BSTA                                                       
         MVC   SVMKT,BMKT                                                       
*                                                                               
VK04     MVC   LKEY,=H'13'                                                      
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'24'                                                  
         MVC   SYSFIL,=C'SPTFILE '                                              
         MVC   SYSDIR,=C'SPTDIR  '                                              
         MVI   USEIO,0                                                          
         XC    FILENAME,FILENAME                                                
*                                                                               
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
*                                                                               
         MVC   SAVEKEY,KEY                                                      
         MVC   AIO,AIO1                                                         
*                                                                               
VKX      J     XIT                                                              
********************************************************************            
* Validate list key                                                             
********************************************************************            
VKL      XC    SVVALS(SVVALSX-SVVALS),SVVALS                                    
*                                                                               
         LA    R2,LSTMEDH           Validate media                              
         CLI   5(R2),0                                                          
         JE    MISSERR                                                          
         GOTOR VALIMED                                                          
         MVC   SVMED,BAGYMD                                                     
*                                                                               
         LA    R2,LSTCLIH           Validate client                             
         CLI   5(R2),0                                                          
         JE    VKL02                                                            
         GOTOR VALICLT                                                          
         MVC   SVCLI,BCLT                                                       
*                                                                               
VKL02    LA    R2,LSTPRDH           Validate product                            
         CLI   5(R2),0                                                          
         JE    VKL04                                                            
         GOTOR VALIPRD                                                          
         MVC   SVPRD,QPRD                                                       
*                                                                               
VKL04    LA    R2,LSTESTH           Validate estimate                           
         CLI   5(R2),0                                                          
         JE    VKL06                                                            
         MVI   SVEST,0              Default = ALL                               
         CLC   =C'ALL',8(R2)                                                    
         JE    VKL06                                                            
         GOTOR VALIEST                                                          
         MVC   SVEST,BEST                                                       
*                                                                               
VKL06    LA    R2,LSTNETH           Validate network                            
         CLI   5(R2),0                                                          
         JE    VKL08                                                            
         MVC   SVSTA,=X'FFFFFF'     Default = ALL                               
         CLC   =C'ALL',8(R2)                                                    
         JE    VKL08                                                            
         GOTOR VALINTWK                                                         
         MVC   SVSTA,BSTA                                                       
         MVC   SVMKT,BMKT                                                       
*                                                                               
VKL08    MVC   LKEY,=H'13'                                                      
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'24'                                                  
         MVC   SYSFIL,=C'SPTFILE '                                              
         MVC   SYSDIR,=C'SPTDIR  '                                              
         MVI   USEIO,0                                                          
         XC    FILENAME,FILENAME                                                
*                                                                               
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
*                                                                               
         MVC   SAVEKEY,KEY                                                      
         MVC   AIO,AIO1                                                         
*                                                                               
VKLX     J     XIT                                                              
***********************************************************************         
* Validate record                                                               
***********************************************************************         
VR       L     R6,AIO                                                           
         USING PXCRECD,R6                                                       
         MVC   PXCKEY,KEY           Build record from scratch                   
         MVC   PXCAGYA,AGENCY                                                   
         MVC   PXCLEN,=X'0018'                                                  
*                                                                               
         MVI   PXCEL01,PXCEL01Q     Build X'01' elem                            
         MVI   PXCEL01+1,X'05'                                                  
         GOTOR DATCON,DMCB,(5,0),(3,PXCACDAT)                                   
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,PXCLEN                                                      
         AHI   RF,5                                                             
         STCM  RF,3,PXCLEN                                                      
*                                                                               
         GOTOR HELLO,DMCB,(C'D',=C'SPTFILE'),(X'05',AIO)                        
*                                                                               
         LA    R2,PXCPRG1H                                                      
         CLI   5(R2),0                                                          
         JE    MISSERR                                                          
*                                                                               
VR10     LA    RF,PXCENDH                                                       
         CR    R2,RF                                                            
         JNL   VR30                                                             
         CLI   5(R2),0                                                          
         JE    VR30                                                             
*                                                                               
E        USING PXCEL05,ELEM                                                     
         XC    ELEM,ELEM                                                        
         MVI   E.PXCEL05,PXCEL05Q                                               
         MVI   E.PXC05LEN,X'17'                                                 
*                                                                               
         CLC   =C'D/',8(R2)         Test day                                    
         JE    VR12                                                             
         CLC   =C'T/',8(R2)         Test time                                   
         JE    VR14                                                             
         J     VR20                                                             
*                                                                               
VR12     ZIC   R3,5(R2)                                                         
         SHI   R3,2                                                             
         MVC   DMCB+4(4),=X'D9000A03'                                           
         GOTOR CALLOV,DMCB,0        DAYVAL                                      
         L     RF,DMCB                                                          
         GOTOR (RF),DMCB,((R3),10(R2)),WORK,WORK+16                             
         CLI   WORK,0                                                           
         JE    INVERR                                                           
         MVI   E.PXCPGM,X'01'                                                   
         MVC   E.PXCPGM+1(1),WORK   Set day                                     
         J     VR22                                                             
*                                                                               
VR14     LA    R3,10(R2)                                                        
VR15     CLI   0(R3),C'/'                                                       
         JE    VR16                                                             
         CLI   0(R3),C' '                                                       
         JNH   VR16                                                             
         AHI   R3,1                                                             
         J     VR15                                                             
*                                                                               
VR16     LA    RF,10(R2)                                                        
         SR    R3,RF                                                            
         MVC   DMCB+4(4),=X'D9000A0E'                                           
         GOTOR CALLOV,DMCB,0        TIMVAL                                      
         L     RF,DMCB                                                          
         GOTOR (RF),DMCB,((R3),10(R2)),WORK                                     
         CLI   DMCB,X'FF'                                                       
         JE    INVERR                                                           
         MVC   E.PXCPGM+1(4),WORK   Set time                                    
         J     VR22                                                             
*                                                                               
VR20     MVC   E.PXCPGM,8(R2)       Set program                                 
         OC    E.PXCPGM,SPACES                                                  
*                                                                               
VR22     ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
*                                                                               
         XC    WORK,WORK                                                        
         LA    R3,8(R2)                                                         
         GOTOR DATVAL,DMCB,(0,(R3)),WORK                                        
         OC    DMCB(4),DMCB                                                     
         JZ    INVERR                                                           
         A     R3,DMCB                                                          
         CLI   0(R3),C'-'           Test 2nd date                               
         JNE   VR24                                                             
         AHI   R3,1                                                             
         GOTOR DATVAL,DMCB,(0,(R3)),WORK+6                                      
         OC    DMCB(4),DMCB                                                     
         JZ    INVERR                                                           
*                                                                               
VR24     GOTOR DATCON,DMCB,(0,WORK),(2,E.PXCSTDTE)                              
         CLI   WORK+6,0                                                         
         JE    VR26                                                             
         GOTOR DATCON,DMCB,(0,WORK+6),(2,E.PXCEDTE)                             
*                                                                               
VR26     GOTOR HELLO,DMCB,(C'P',=C'SPTFILE'),(X'05',AIO),ELEM,0                 
*                                                                               
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         J     VR10                                                             
*                                                                               
VR30     J     XIT                                                              
***********************************************************************         
* Display record                                                                
***********************************************************************         
DR       GOTOR CLRSCRN              Clear screen for display                    
*                                                                               
         L     R6,AIO                                                           
         LA    R2,PXCPRG1H                                                      
*                                                                               
         MVI   ELCODE,PXCEL05Q                                                  
         BAS   RE,GETEL                                                         
         J     *+8                                                              
DR10     BAS   RE,NEXTEL                                                        
         JNE   DRX                                                              
         USING PXCEL05,R6                                                       
*                                                                               
         LA    RF,PXCENDH           Test end of screen                          
         CR    R2,RF                                                            
         JNL   DRX                                                              
         OI    6(R2),X'80'                                                      
*                                                                               
         CLI   PXCPGM,X'01'         Test day                                    
         JE    DR12                                                             
         CLI   PXCPGM,0             Test time                                   
         JE    DR14                                                             
         J     DR18                                                             
*                                                                               
DR12     MVC   8(2,R2),=C'D/'                                                   
         MVC   DMCB+4(4),=X'D9000A0F'                                           
         GOTOR CALLOV,DMCB,0        DAYUNPK                                     
         L     RF,DMCB                                                          
         GOTOR (RF),DMCB,PXCPGM+1,10(R2)                                        
         J     DR20                                                             
*                                                                               
DR14     MVC   8(2,R2),=C'T/'                                                   
         MVC   DMCB+4(4),=X'D9000A11'                                           
         GOTOR CALLOV,DMCB,0        UNTIME                                      
         L     RF,DMCB                                                          
         GOTOR (RF),DMCB,PXCPGM+1,10(R2)                                        
         J     DR20                                                             
*                                                                               
DR18     MVC   8(L'PXCPGM,R2),PXCPGM                                            
*                                                                               
DR20     ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         OI    6(R2),X'80'                                                      
         GOTOR DATCON,DMCB,(2,PXCSTDTE),(5,8(R2))                               
         CLI   PXCEDTE,0                                                        
         JE    DR22                                                             
         MVI   16(R2),C'-'                                                      
         GOTOR DATCON,DMCB,(2,PXCEDTE),(5,17(R2))                               
*                                                                               
DR22     ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         J     DR10                                                             
*                                                                               
DRX      MVC   AIO,AIO1                                                         
         J     XIT                                                              
***********************************************************************         
* Display key                                                                   
***********************************************************************         
DK       L     R6,AIO                                                           
         USING PXCRECD,R6                                                       
*                                                                               
         MVI   PXCMED,C'N'                                                      
         OI    PXCMEDH+6,X'80'                                                  
*                                                                               
         GOTO1 CLUNPK,DMCB,(BCLIAAN,PXCKCLT),PXCCLI                             
         OI    PXCCLIH+6,X'80'                                                  
*                                                                               
         MVC   PXCPRD,PXCKPRD                                                   
         OI    PXCPRDH+6,X'80'                                                  
*                                                                               
         MVC   PXCNET,=C'ALL '                                                  
         OI    PXCNETH+6,X'80'                                                  
         CLC   PXCKSTA,=X'FFFFFF'                                               
         JE    DK02                                                             
         XC    WORK,WORK                                                        
         MVC   WORK+2(L'PXCKSTA),PXCKSTA                                        
         GOTO1 VMSUNPK,DMCB,WORK,WORK+5,WORK+10                                 
         MVC   PXCNET,WORK+10                                                   
*                                                                               
DK02     MVC   PXCEST,=C'ALL'                                                   
         OI    PXCESTH+6,X'80'                                                  
         CLI   PXCKEST,0                                                        
         JE    DKX                                                              
         EDIT  PXCKEST,PXCEST,ALIGN=LEFT                                        
*                                                                               
DKX      J     XIT                                                              
***********************************************************************         
* List records                                                                  
***********************************************************************         
LR       GOTOR CHKKEY                                                           
*                                                                               
         CLI   KEY,0                First time through?                         
         JNE   *+10                                                             
         MVC   PREVKEY,SAVEKEY                                                  
*                                                                               
LR10     XC    KEY,KEY                                                          
         MVC   KEY(L'PXCKEY),PREVKEY                                            
         GOTO1 HIGH                                                             
         J     LR20                                                             
*                                                                               
LRSEQ    GOTO1 SEQ                                                              
*                                                                               
LR20     CLC   KEY(PXCKCLT-PXCKEY),KEYSAVE                                      
         JNE   LRX                                                              
*                                                                               
         LA    R6,KEY                                                           
         USING PXCRECD,R6                                                       
*                                                                               
         OC    SVCLI,SVCLI                                                      
         JZ    *+14                                                             
         CLC   PXCKCLT,SVCLI                                                    
         JNE   LRSEQ                                                            
*                                                                               
         OC    SVSTA,SVSTA                                                      
         JZ    *+14                                                             
         CLC   PXCKSTA,SVSTA                                                    
         JNE   LRSEQ                                                            
*                                                                               
         OC    SVPRD,SVPRD                                                      
         JZ    *+14                                                             
         CLC   PXCKPRD,SVPRD                                                    
         JNE   LRSEQ                                                            
*                                                                               
         CLC   LSTEST,=C'ALL'                                                   
         JNE   LR22                                                             
         CLI   PXCKEST,0            All?                                        
         JNE   LRSEQ                                                            
         J     LR24                                                             
*                                                                               
LR22     CLI   SVEST,0                                                          
         JE    *+14                                                             
         CLC   PXCKEST,SVEST                                                    
         JNE   LRSEQ                                                            
*                                                                               
LR24     CLC   LSTNET,=C'ALL'                                                   
         JNE   LR26                                                             
         CLC   PXCKSTA,=X'FFFFFF'   All?                                        
         JNE   LRSEQ                                                            
         J     LR28                                                             
*                                                                               
LR26     OC    SVSTA,SVSTA                                                      
         JZ    *+14                                                             
         CLC   PXCKSTA,SVSTA                                                    
         JNE   LRSEQ                                                            
*                                                                               
LR28     MVC   PREVKEY,KEY                                                      
*                                                                               
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
*                                                                               
         LA    R5,LISTAR                                                        
         USING PLINED,R5                                                        
         L     R6,AIO                                                           
         USING PXCRECD,R6                                                       
*                                                                               
         GOTOR CLUNPK,DMCB,PXCKCLT,PCLI                                         
         MVC   PPRD,PXCKPRD                                                     
*                                                                               
         MVC   PNET,=C'ALL '                                                    
         CLC   PXCKSTA,=X'FFFFFF'                                               
         JE    LR30                                                             
         XC    WORK,WORK                                                        
         MVC   WORK+2(L'PXCKSTA),PXCKSTA                                        
         GOTO1 VMSUNPK,DMCB,WORK,WORK+5,WORK+10                                 
         MVC   PNET,WORK+10                                                     
*                                                                               
LR30     MVC   PEST,=C'ALL'                                                     
         CLI   PXCKEST,0                                                        
         JE    LR40                                                             
         EDIT  PXCKEST,PEST,ALIGN=LEFT                                          
*                                                                               
LR40     GOTO1 LISTMON                                                          
         J     LRSEQ                                                            
*                                                                               
LRX      J     XIT                                                              
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
CHKKEYX  J     XIT                                                              
*********************************************************************           
* Clear screen for display                                                      
*********************************************************************           
CLRSCRN  NTR1                                                                   
         LA    R2,PXCPRG1H                                                      
         LA    R3,PXCENDH                                                       
*                                                                               
CLRS02   CR    R2,R3                                                            
         JE    CLRSCRNX                                                         
         XC    8(L'PXCPRG1,R2),8(R2)                                            
         OI    6(R2),X'80'                                                      
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         J     CLRS02                                                           
*                                                                               
CLRSCRNX J     XIT                                                              
*********************************************************************           
MISSERR  MVI   ERROR,MISSING                                                    
         J     TRAPERR                                                          
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
         J     TRAPERR                                                          
*                                                                               
NOTAUTH  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(21),=C'ACTION NOT AUTHORIZED'                            
         B     MSGERR                                                           
*                                                                               
MSGERR   MVI   ERROR,0                                                          
         GOTO1 ERREX2                                                           
*                                                                               
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
*                                                                               
TRAPEND  MVI   ERROPT,0                                                         
         GOTOR ERREX                                                            
*                                                                               
         GETEL R6,24,ELCODE                                                     
*                                                                               
         DS    0F                                                               
ZEROES   DC    20C'0'                                                           
ERRDISP  DS    H                                                                
RELO     DS    A                                                                
         LTORG                                                                  
         DROP  R7,RB                                                            
*                                                                               
       ++INCLUDE NESFMFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE DDGENTWA                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NESFM9AD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NESFM80D                                                       
*                                                                               
       ++INCLUDE NESFMWORKD                                                     
       ++INCLUDE SPGENPXC                                                       
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE SPDDEQUS                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FATIOB                                                         
*                                                                               
GEND     DSECT                                                                  
SPOOLD   DSECT                                                                  
SYSD     DSECT                                                                  
         ORG   SYSSPARE+220                                                     
*                                                                               
SAVEKEY  DS    CL(L'PXCKEY)                                                     
PREVKEY  DS    CL(L'PXCKEY)                                                     
*                                                                               
SVVALS   DS    0X                                                               
SVMED    DS    XL1                                                              
SVCLI    DS    XL2                                                              
SVSTA    DS    XL3                                                              
SVPRD    DS    CL3                                                              
SVEST    DS    XL1                                                              
SVMKT    DS    CL4                                                              
SVVALSX  DS    0X                                                               
*                                                                               
TEMP     DS    X                                                                
SCANTBL  DS    XL256                                                            
NLINES   DS    X                                                                
FNDX     DS    X                                                                
MYKEY    DS    CL(L'KEY)                                                        
*                                                                               
PLINED   DSECT                                                                  
PCLI     DS    CL3                                                              
         DS    CL2                                                              
PPRD     DS    CL3                                                              
         DS    CL2                                                              
PEST     DS    CL3                                                              
         DS    CL2                                                              
PNET     DS    CL4                                                              
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002NESFM56   06/04/08'                                      
         END                                                                    
