*          DATA SET SPSFM47    AT LEVEL 028 AS OF 03/14/02                      
*PHASE T21747A                                                                  
***********************************************************************         
*                                                                     *         
*  TITLE: SPSFM47 - DISPLAY INCH MINIO RECORDS USING DATAMGR          *         
*                                                                     *         
*  COMMENTS: DISPLAY AND LIST INCH RECORDS                            *         
*                                                                     *         
*  CALLED FROM: SFM CONTROLLER (T21700), WHICH CALLS                  *         
*                 GENCON (T00A30) WHICH CALLS THIS.                   *         
*                                                                     *         
*  CALLS TO: DATAMGR                                                  *         
*                                                                     *         
*  INPUTS: SCREENS SCSFM7C (T2977C) -- MAINTENANCE                    *         
*                  SCSFM7D (T2977D) -- LIST                           *         
*                                                                     *         
*  OUTPUTS: CURRENT INCH RECORDS                                      *         
*                                                                     *         
*  LOCALS: REGISTER USAGE                                             *         
*          R0 - WORK                                                  *         
*          R1 - WORK                                                  *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CURSOR    *         
*          R3 - GETEL REGISTER                                        *         
*          R4 - WORK                                                  *         
*          R5 - WORK                                                  *         
*          R6 - WORK                                                  *         
*          R7 - 2ND BASE                                              *         
*          R8 - SPOOLD                                                *         
*          R9 - SYSD                                                  *         
*          RA - TWA                                                   *         
*          RB - FIRST BASE                                            *         
*          RC - GEND                                                  *         
*          RD - SYSTEM                                                *         
*          RE - SYSTEM                                                *         
*          RF - SYSTEM                                                *         
*                                                                     *         
***********************************************************************         
         TITLE 'T21747  DISPLAY INCH MINIO RECORDS'                             
T21747   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21747,R7                                                      
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         BAS   RE,SELPFM                                                        
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,DISPKEY        DISPLAY RECORD                               
         BE    DK                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,SETFILE        SET THE FILE TO BE READ                      
         BE    SF                                                               
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*===========================================================*                   
*                  SET FILE ROUTINE                         *                   
*===========================================================*                   
SF       MVI   RDFLAG,RDXSP        SET TO XSPDIR, XSPFIL                        
         BAS   RE,SETFLS                                                        
         B     XIT                                                              
*===========================================================*                   
*                  VALIDATE KEY ROUTINE                     *                   
*===========================================================*                   
VK       MVI   RDFLAG,RDSPT        SET TO SPTDIR, SPTFIL                        
         BAS   RE,SETFLS                                                        
*                                                                               
         LA    R3,TEMPKEY                                                       
         USING MSRKEYD,R3                                                       
*                                                                               
         XC    TEMPKEY,TEMPKEY                                                  
         MVI   MSRKTYPE,MSRKTYPQ   X'0E'                                        
         MVI   MSRKSUB,MSRKSUBQ    X'04'                                        
*                                                                               
         LA    R2,ICHMEDH          AGENCY/MEDIA                                 
         GOTO1 VALIMED                                                          
         MVC   MSRKAM,BAGYMD                                                    
*                                                                               
         CLI   ACTNUM,ACTLIST                                                   
         BE    VKL                                                              
*                                                                               
         LA    R2,ICHCLIH          CLIENT                                       
         GOTO1 VALICLT                                                          
         MVC   MSRKCLT,BCLT                                                     
*                                                                               
         LA    R2,ICHPROH          PRODUCT                                      
         GOTO1 VALIPRD                                                          
         MVC   MSRKPRD,BPRD                                                     
*                                                                               
         LA    R2,ICHESTH          ESTIMATE                                     
         GOTO1 VALIEST                                                          
         MVC   MSRKEST,BEST                                                     
*                                                                               
         LA    R2,ICHMKTH          MARKET                                       
         GOTO1 VALIMKT                                                          
         MVC   MSRKMKT,BMKT                                                     
*                                                                               
         LA    R2,ICHSTAH          STATION                                      
         GOTO1 VALISTA                                                          
         MVC   MSRKSTA,BSTA                                                     
*                                                                               
VK02     LA    R2,ICHMOSH          MONTH OF SERVICE DATE                        
         CLI   5(R2),6                                                          
         BNE   VK05                                                             
*                                                                               
         MVC   OTMPDT,ICHMOS       STICK IN A ONE                               
         MVC   NTMPMON,OTMPMON      (JAN/96 - TO - JAN01/96)                    
         MVC   NTMPDAY,=C'01'                                                   
         MVC   NTMPYR,OTMPYR                                                    
         GOTO1 DATVAL,DMCB,(0,NTMPDT),DCTMPDT                                   
         OC    DMCB,DMCB           IS DATE VALID?                               
         BNZ   VK10                YES                                          
*                                                                               
VK05     MVI   ERROR,INVDATE                                                    
         GOTO1 ERREX                                                            
*                                                                               
VK10     GOTO1 DATCON,DMCB,(0,DCTMPDT),(2,MSRKMOS)                              
         XC    MSRKMOS,=X'FFFF'    COMPLEMENT FOR DESCENDING ORDER              
*                                                                               
VK20     MVI   RDFLAG,RDXSP        SET FILES TO XSPDIR/FIL                      
         BAS   RE,SETFLS                                                        
         CLI   ACTNUM,ACTLIST                                                   
         BE    VKX                                                              
         MVC   KEY,TEMPKEY                                                      
         GOTO1 HIGH                READ HIGH TO GET FULL KEY                    
         CLC   KEY(L'MSRKMAST),KEYSAVE    IS IT A MATCH?                        
         BE    VKX                        YES                                   
*                                                                               
         MVI   ERROR,NOTFOUND                                                   
         LA    R2,ICHMEDH                                                       
         GOTO1 ERREX                                                            
*                                                                               
VKX      B     XIT                                                              
         EJECT                                                                  
*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - -                    
*                  VALIDATE THE KEY FOR A LIST                                  
*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - -                    
VKL      DS    0H                                                               
         LA    R2,ICHCLIH          CLIENT                                       
         CLI   5(R2),0             IS THERE ONE?                                
         BE    VKL20               NO                                           
         GOTO1 VALICLT                                                          
         MVC   MSRKCLT,BCLT                                                     
*                                                                               
         LA    R2,ICHPROH          PRODUCT                                      
         CLI   5(R2),0                                                          
         BE    VKL25                                                            
         GOTO1 VALIPRD                                                          
         MVC   MSRKPRD,BPRD                                                     
*                                                                               
         LA    R2,ICHESTH          ESTIMATE                                     
         CLI   5(R2),0                                                          
         BE    VKL30                                                            
         GOTO1 VALIEST                                                          
         MVC   MSRKEST,BEST                                                     
         B     VKL30                                                            
*                                                                               
VKL20    CLI   ICHPROH+5,0         ERROR IF PRODUCT AND NO CLIENT               
         BNE   VKLERR                                                           
VKL25    CLI   ICHESTH+5,0                                                      
         BNE   VKLERR                                                           
*                                                                               
VKL30    LA    R2,ICHMKTH          MARKET                                       
         CLI   5(R2),0                                                          
         BE    VKL40                                                            
         GOTO1 VALIMKT                                                          
         MVC   MSRKMKT,BMKT                                                     
*                                                                               
         LA    R2,ICHSTAH          STATION                                      
         CLI   5(R2),0                                                          
         BE    VKL45                                                            
         GOTO1 VALISTA                                                          
         MVC   MSRKSTA,BSTA                                                     
         B     VKL45                                                            
*                                                                               
VKL40    CLI   ICHSTAH+5,0                                                      
         BNE   VKLERR                                                           
*                                                                               
VKL45    LA    R2,ICHMOSH          MONTH OF SERVICE                             
         CLI   5(R2),0                                                          
         BE    VKLX                                                             
         B     VK02                                                             
*                                                                               
VKLERR   MVI   ERROR,MISSING                                                    
         GOTO1 ERREX                                                            
*                                                                               
VKLX     B     VK20                                                             
         DROP  R3                                                               
         EJECT                                                                  
*==============================================================*                
*                     DISPLAY  RECORD                          *                
*==============================================================*                
DR       BAS   RE,CLRFLDS                                                       
*                                                                               
         GOTO1 HEXOUT,DMCB,KEY+36,ICHDADD,4                                     
*                                                                               
         L     R3,AIO                                                           
         MVI   ELCODE,MSRSTELQ     X'10'                                        
         BAS   RE,GETEL                                                         
         BNE   DR10                                                             
         USING MSRSTELD,R3                                                      
*                                                 RUN DATE                      
         OC    MSRSTDAT,MSRSTDAT                                                
         BZ    DR005                                                            
         GOTO1 DATCON,DMCB,(0,MSRSTDAT),(11,ICHRNDT)                            
         OI    ICHRNDTH+6,X'80'                                                 
*                                                 STATUS                        
DR005    MVC   ICHSTTS,MSRSTSTA                                                 
         OI    ICHSTTSH+6,X'80'                                                 
*                                                 SPOTS INVOICE                 
         EDIT  MSRSTINS,ICHIVSP,ALIGN=LEFT                                      
         OI    ICHIVSPH+6,X'80'                                                 
*                                                 SP GROSS                      
         EDIT  MSRSTING,ICHIVGR,2,ALIGN=LEFT,FLOAT=$,COMMAS=YES                 
         OI    ICHIVGRH+6,X'80'                                                 
*                                                 ERROR CODES                   
*        MVC   ICHERRC,MSRSTERS                                                 
*        OI    ICHERRCH+6,X'80'                                                 
*                                                 BUY LOCKIN STATUS             
         TM    MSRSTBGS,MSRBGUSD   USE MSRSTBGS                                 
         BO    DR06                                                             
         GOTO1 HEXOUT,DMCB,MSRSTBLS,ICHBLST,L'MSRSTBLS                          
         GOTO1 HEXOUT,DMCB,MSRMPERR,ICHBGST,L'MSRMPERR                          
         B     DR07                                                             
DR06     GOTO1 HEXOUT,DMCB,MSRSTBGS,ICHBLST,L'MSRSTBGS                          
         GOTO1 HEXOUT,DMCB,MSRMPERR,ICHBGST,L'MSRMPERR                          
DR07     OI    ICHBLSTH+6,X'80'                                                 
         OI    ICHBGSTH+6,X'80'                                                 
*                                                                               
DR10     L     R3,AIO                                                           
         MVI   ELCODE,MSRICELQ     X'13'                                        
         BAS   RE,GETEL                                                         
         BNE   DR20                                                             
         USING MSRICELD,R3                                                      
*                                                    INCH DATE                  
         OC    MSRICDAT,MSRICDAT                                                
         BZ    DR010                                                            
         GOTO1 DATCON,DMCB,(0,MSRICDAT),(11,ICHINDT)                            
         OI    ICHINDTH+6,X'80'                                                 
*                                                    INCH STATUS                
DR010    MVC   ICHINST,MSRICST                                                  
         OI    ICHINSTH+6,X'80'                                                 
         GOTO1 HEXOUT,DMCB,MSRICST2,ICHIST2,L'MSRICST2                          
         OI    ICHIST2H+6,X'80'                                                 
*                                                                               
         MVC   ICHICHN,MSRICPER                                                 
         OI    ICHICHNH+6,X'80'                                                 
*                                                                               
DR20     L     R3,AIO                                                           
         MVI   ELCODE,MSRIPELQ     X'14'                                        
         BAS   RE,GETEL                                                         
         BNE   DR30                                                             
         USING MSRIPELD,R3                                                      
*                                                      PAY DATE                 
         OC    MSRIPDAT,MSRIPDAT                                                
         BZ    DR015                                                            
         GOTO1 DATCON,DMCB,(0,MSRIPDAT),(11,ICHPNDT)                            
         OI    ICHPNDTH+6,X'80'                                                 
*                                                      PAY STATUS               
DR015    MVC   ICHPNST,MSRIPST                                                  
         OI    ICHPNSTH+6,X'80'                                                 
         MVC   ICHPNAM,MSRIPPER                                                 
         OI    ICHPNAMH+6,X'80'                        PAY NAME                 
*                                                                               
DR30     L     R3,AIO                                                           
         MVI   ELCODE,MSRINELQ     X'12'                                        
         BAS   RE,GETEL                                                         
         BNE   DRX                                                              
         USING MSRINELD,R3                                                      
*                                                                               
         LA    R2,ICHLINFH                                                      
         USING DELINED,R2                                                       
*                                       INVOICE DATE AND 'DATE ADDED'           
DR35     MVC   DLINVNUM(L'MSRININO),MSRININO                                    
         EDIT  MSRINAMT,DLINVAMT,2,ALIGN=LEFT,FLOAT=$,COMMAS=YES                
         CLC   =X'404040',MSRINIDT                                              
         BE    DR36                                                             
         CLC   =6X'00',MSRINIDT         SJR/TST HAS SOME FUNNY REC'S            
         BNE   *+14                     THAT SCREWS UP DATCON                   
DR36     MVC   DLINVDT,SPACES                                                   
         B     DR37                                                             
         GOTO1 DATCON,DMCB,(0,MSRINIDT),(11,DLINVDT)                            
DR37     GOTO1 DATCON,DMCB,(2,MSRINADT),(11,DLADDDT)                            
*                                                   IS INVOICE PAID?            
         MVI   DLPAID,C'N'                                                      
         TM    MSRINST,MSRINPYQ    X'80'                                        
         BZ    *+8                                                              
         MVI   DLPAID,C'Y'                                                      
                                                                                
         OI    6(R2),X'80'         TRANSMIT DETAIL LINE                         
*                                                                               
         BAS   RE,NEXTEL                                                        
         BNE   DRX                                                              
         BAS   RE,BMPFLD                                                        
         B     DR35                                                             
*                                                                               
DRX      B     XIT                                                              
         DROP  R2                                                               
         DROP  R3                                                               
         EJECT                                                                  
*==============================================================*                
*                     DISPLAY  KEY                             *                
*==============================================================*                
DK       L     R3,AIO                                                           
         USING MSRKEYD,R3                                                       
*                                                                               
         GOTO1 CLUNPK,DMCB,MSRKCLT,ICHCLI                                       
         OI    ICHCLIH+6,X'80'                                                  
         BAS   RE,RTPRNM                                                        
         MVC   ICHPRO(L'TEMPPRD),TEMPPRD                                        
         OI    ICHPROH+6,X'80'                                                  
*                                                                               
         GOTO1 MSUNPK,DMCB,MSRKMKT,ICHMKT,ICHSTA                                
         OI    ICHSTAH+6,X'80'                                                  
         EDIT  MSRKEST,ICHEST,ALIGN=LEFT                                        
         OI    ICHESTH+6,X'80'                                                  
*                                                                               
         XC    MSRKMOS,=X'FFFF'                                                 
         GOTO1 DATCON,DMCB,(2,MSRKMOS),(11,NTMPDT)                              
         MVC   OTMPMON,NTMPMON                                                  
         MVC   OTMPYR,NTMPYR                                                    
         MVC   ICHMOS,OTMPDT                                                    
         OI    ICHMOSH+6,X'80'                                                  
*                                                                               
DKX      B     XIT                                                              
         EJECT                                                                  
         DROP  R3                                                               
*=================================================================*             
*                         LIST RECORDS                            *             
*=================================================================*             
LR       LA    R4,KEY                                                           
         USING MSRKEYD,R4                                                       
*                                                                               
         OC    KEY,KEY                                                          
         BNZ   LR10                                                             
         MVC   KEY,TEMPKEY                                                      
         MVC   SAVEKEY,TEMPKEY                                                  
*                                                                               
LR10     GOTO1 HIGH                STARTING INCH REC IN AIO1 AND 2              
         B     LR20                                                             
*                                                                               
LR15     GOTO1 SEQ                                                              
LR20     CLC   KEY(3),SAVEKEY      ARE THERE STILL INCH RECS?                   
         BNE   LRX                 NO                                           
         CLC   KEY(L'MSRKMAST),SAVEKEY   MORE WITH SAME MASTER KEY?             
         BE    LR15                      YES, SKIP THEM                         
*                                                                               
         MVC   MYKEY,KEY                                                        
         MVI   FILTFLAG,0                                                       
         BAS   RE,FILTERS                                                       
         TM    FILTFLAG,FLTSKIP    X'80'    RECORD TO BE FILTERED OUT?          
         BO    LR15                         YES                                 
*                                                                               
         GOTO1 GETREC                                                           
         XC    LISTAR,LISTAR                                                    
*                                                                               
         GOTO1 CLUNPK,DMCB,MSRKCLT,LSTCLT                                       
         BAS   RE,RTPRNM           RETURN THE PRODUCT                           
         MVC   LSTPRD,TEMPPRD                                                   
         GOTO1 MSUNPK,DMCB,MSRKMKT,LSTMKT,LSTSTA                                
         EDIT  MSRKEST,LSTEST                                                   
*                                                                               
         XC    MSRKMOS,=X'FFFF'    MONTH OF SERVICE DATE                        
         GOTO1 DATCON,DMCB,(2,MSRKMOS),(11,NTMPDT)                              
         MVC   OTMPMON,NTMPMON                                                  
         MVC   OTMPYR,NTMPYR                                                    
         MVC   LSTMOS,OTMPDT                                                    
*                                                                               
         L     R3,AIO                                                           
         MVI   ELCODE,X'13'                                                     
         BAS   RE,GETEL                                                         
         BNE   LR30                                                             
         USING MSRICELD,R3                                                      
         MVC   LSTSTAT,MSRICST                                                  
         DROP  R3                                                               
*                                                                               
LR30     GOTO1 LISTMON                                                          
         MVC   KEY,MYKEY                                                        
         MVC   SAVEKEY,KEY                                                      
         B     LR15                                                             
*                                                                               
LRX      B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*=================================================================*             
*                    CHECK THE FILTERS                            *             
*=================================================================*             
FILTERS  NTR1                                                                   
         LA    R4,TEMPKEY                                                       
         USING MSRKEYD,TEMPKEY              FILTER BY:                          
*                                                                               
         OC    MSRKCLT,MSRKCLT               CLIENT?                            
         BZ    FLT20                                                            
         CLC   MSRKCLT,KEY+MSRKCLT-MSRKEY                                       
         BNE   FLT100                                                           
*                                                                               
         OC    MSRKPRD,MSRKPRD               PRODUCT?                           
         BZ    FLT20                                                            
         CLC   MSRKPRD,KEY+MSRKPRD-MSRKEY                                       
         BNE   FLT100                                                           
*                                                                               
         OC    MSRKEST,MSRKEST               ESTIMATE?                          
         BZ    FLT20                                                            
         CLC   MSRKEST,KEY+MSRKEST-MSRKEY                                       
         BNE   FLT100                                                           
*                                                                               
FLT20    OC    MSRKMKT,MSRKMKT               MARKET?                            
         BZ    FLT30                                                            
         CLC   MSRKMKT,KEY+MSRKMKT-MSRKEY                                       
         BNE   FLT100                                                           
*                                                                               
         OC    MSRKSTA,MSRKSTA               STATION?                           
         BZ    FLT30                                                            
         CLC   MSRKSTA,KEY+MSRKSTA-MSRKEY                                       
         BNE   FLT100                                                           
*                                                                               
FLT30    OC    MSRKMOS,MSRKMOS               MONTH OF SERVICE?                  
         BZ    FLTX                                                             
         CLC   MSRKMOS,KEY+MSRKMOS-MSRKEY                                       
         BNE   FLT100                                                           
         B     FLTX                                                             
*                                                                               
FLT100   OI    FILTFLAG,FLTSKIP   TURNED ON TO FILTER THIS RECORD OUT           
FLTX     B     XIT                                                              
*=================================================================*             
*                    CLEAR DISPLAY FIELDS                         *             
*=================================================================*             
CLRFLDS  NTR1                                                                   
         XC    ICHDADD,ICHDADD                                                  
         OI    ICHDADDH+6,X'80'                                                 
         XC    ICHRNDT,ICHRNDT                                                  
         OI    ICHRNDTH+6,X'80'                                                 
         XC    ICHSTTS,ICHSTTS                                                  
         OI    ICHSTTSH+6,X'80'                                                 
         XC    ICHIVSP,ICHIVSP                                                  
         OI    ICHIVSPH+6,X'80'                                                 
         XC    ICHBLST,ICHBLST                                                  
         OI    ICHBLSTH+6,X'80'                                                 
         XC    ICHIVGR,ICHIVGR                                                  
         OI    ICHIVGRH+6,X'80'                                                 
         XC    ICHINST,ICHINST                                                  
         OI    ICHINSTH+6,X'80'                                                 
         XC    ICHIST2,ICHIST2                                                  
         OI    ICHIST2H+6,X'80'                                                 
         XC    ICHINDT,ICHINDT                                                  
         OI    ICHINDTH+6,X'80'                                                 
         XC    ICHPNST,ICHPNST                                                  
         OI    ICHPNSTH+6,X'80'                                                 
         XC    ICHPNDT,ICHPNDT                                                  
         OI    ICHPNDTH+6,X'80'                                                 
*                                                                               
         LA    R2,ICHLINFH         FIRST DETAIL LINE FIELD HEADER               
         LA    R5,ICHLINLH         LAST DETAIL LINE FIELD HEADER                
*                                                                               
CF10     XC    8(L'ICHLINF,R2),8(R2)                                            
         OI    6(R2),X'80'                                                      
         CR    R2,R5                                                            
         BE    CFX                                                              
         BAS   RE,BMPFLD                                                        
         B     CF10                                                             
*                                                                               
CFX      B     XIT                                                              
*===================================================================*           
*              SET VARIABLES TO READ XSPDIR/FIL FILES               *           
*===================================================================*           
SETFLS   CLI   RDFLAG,RDSPT        ARE WE SET FOR XSP FILES?                    
         BE    SETFLSBK            YES, THEN RESET TO SPT FILES                 
*                                                                               
         MVC   LKEY,=H'32'         SET TO READ XSPDIR/FIL FILES                 
         MVC   LSTATUS,=H'4'                                                    
         MVC   DATADISP,=H'42'                                                  
         MVC   SYSFIL,=C'XSPFIL  '                                              
         MVC   SYSDIR,=C'XSPDIR  '                                              
         B     SETFLSX                                                          
*                                                                               
SETFLSBK MVC   LKEY,=H'13'         SET TO READ SPTDIR/FIL FILES                 
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'24'                                                  
         MVC   SYSFIL,=C'SPTFIL  '                                              
         MVC   SYSDIR,=C'SPTDIR  '                                              
*                                                                               
SETFLSX  MVI   RDFLAG,0                                                         
         BR    RE                                                               
         EJECT                                                                  
*==================================================================*            
*                 BUMP TO THE NEXT FIELD                           *            
*==================================================================*            
BMPFLD   ZIC  R0,0(R2)                                                          
         AR   R2,R0                                                             
         BR   RE                                                                
*===================================================================*           
*                  RETRIEVE THE PRODUCT NAME                        *           
*===================================================================*           
RTPRNM   NTR1                                                                   
         MVI   RDFLAG,RDSPT                                                     
         BAS   RE,SETFLS                                                        
                                                                                
         L     R4,AIO                                                           
         USING MSRKEYD,R4                                                       
*                                                                               
         MVC   AIO,AIO3            SAVE EVERYTHING                              
         MVC   SKEY,KEY                                                         
*                                                                               
         XC    KEY,KEY             SET UP CLIENT RECORD KEY                     
         MVC   KEY+1(1),MSRKAM                                                  
         MVC   KEY+2(2),MSRKCLT                                                 
*                                                                               
         GOTO1 READ                                                             
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO              PULL PROD NAME FROM LIST                     
         USING CLTHDRD,R6                                                       
         LA    R2,CLIST                                                         
*                                                                               
RTPR10   CLC   MSRKPRD,3(R2)       MATCH PRD CODE WITH MNEUMONIC                
         BE    RTPR20                                                           
         LA    R2,4(R2)                                                         
         B     RTPR10                                                           
*                                                                               
RTPR20   MVC   TEMPPRD,0(R2)       MNEUMONIC GOES IN TEMPPRD                    
RTPRX    MVC   AIO,AIO1            RESTORE AIO,KEY,SAVEKEY AND FILES            
         MVI   RDFLAG,RDXSP                                                     
         BAS   RE,SETFLS                                                        
         MVC   KEY,SKEY                                                         
         GOTO1 GETREC              RESTORE POINTER TO PROPER DA                 
         B     XIT                                                              
         DROP  R4                                                               
*        DROP  R6                                                               
*===================================================================*           
*              CHECK SELECT FIELDS FOR A SWITCH TO PFM              *           
*===================================================================*           
SELPFM   NTR1                                                                   
         CLI   ACTNUM,ACTLIST      CHECK FOR PFM ONLY ON LIST                   
         BNE   SELPFMX                                                          
*                                                                               
         LA    R2,ILSSELH          FIRST SELECT FIELD                           
         LA    R5,ILSLAST          FIELD AFTER SELECTS                          
         SR    R6,R6               R6 IS THE COUNTER OF LINES                   
*                                                                               
SP05     CLC   =C'PFM',8(R2)                                                    
         BE    SP10                                                             
         LA    R6,1(R6)                                                         
         BAS   RE,BMPFLD           BUMP PAST SELECT AND INFO LINE               
         BAS   RE,BMPFLD                                                        
         CR    R2,R5                                                            
         BE    SELPFMX                                                          
         B     SP05                                                             
*                                                                               
SP10     MVC   8(3,R2),=C'S  '      CHANGE PFM TO 'S' FOR SELECT                
         OI    4(R2),X'80'         INPUTTED THIS TIME                           
         MH    R6,=H'6'            6 IS LENGTH OF LISTDIR FIELDS                
         LA    R5,LISTDIR          GET DISK ADDR FOR REC TO BE VIEWED           
         LA    R5,0(R6,R5)                                                      
         MVC   SLISTDA,2(R5)                                                    
         BAS   RE,GOTOPFM                                                       
SELPFMX  B     XIT                                                              
*=====================================================================*         
*          CALL PFM THROUGH GLOBBER AND USER RETURNS WITH =SW ($SW)             
*=====================================================================*         
GOTOPFM  NTR1                                                                   
         L     RF,ACOMFACS                                                      
         USING COMFACSD,RF                                                      
         MVC   VGLOBBER,CGLOBBER                                                
         DROP  RF                                                               
         XC    ELEM,ELEM           SET UP GLOBAL ELEMENT                        
         LA    R1,ELEM                                                          
         USING GLPFMFIL,R1                                                      
         MVC   GLPFMFIL(6),=C'XSPFIL'                                           
         MVC   GLPFMDA,SLISTDA                                                  
         MVC   GLPFMKEY(4),=C'*   '                                             
         GOTO1 VGLOBBER,DMCB,=C'PUTD',ELEM,54,GLPFMCDQ                          
*                                                                               
         XC    ELEM,ELEM           SET UP TRANSFER ELEMENT                      
         LA    R1,ELEM                                                          
         USING GLVXFRSY,R1                                                      
         MVC   GLVXFRSY,=C'SPO'                                                 
         MVC   GLVXTOSY,=C'SPO'                                                 
         MVC   GLVXFRPR,=C'SFM'                                                 
         MVC   GLVXTOPR,=C'PFM'                                                 
         OI    GLVXFLG1,GLV1SEPS                                                
         GOTO1 VGLOBBER,DMCB,=C'PUTD',ELEM,14,GLVXCTL                           
GOTOPFMX B     XIT                                                              
*===================================================================*           
         GETEL R3,DATADISP,ELCODE                                               
*===================================================================*           
VGLOBBER DS    V                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
       ++INCLUDE SPSFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM7CD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM7DD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE SPSFMWORKD                                                     
* ---------------------------------------------------------------               
         ORG   SYSSPARE         1K APPLICATION AREA                             
MYKEY    DS    XL48                                                             
SKEY     DS    XL48                                                             
TEMPKEY  DS    XL48                                                             
SAVEKEY  DS    XL48                                                             
TEMPPRD  DS    CL2              VALUE RETURNED FROM RTPRNM SUB ROUT             
SLISTDA  DS    XL4                                                              
*                                                                               
FILTFLAG DS    X                                                                
FLTSKIP  EQU   X'80'                                                            
*                                                                               
RDFLAG   DS    X                                                                
RDXSP    EQU   X'40'                                                            
RDSPT    EQU   X'04'                                                            
* --- USED FOR CONVERTING THE DATES (EX. JAN/96 -TO- JAN01/96)                  
OTMPDT   DS    0CL6                                                             
OTMPMON  DS    CL3                                                              
OTMPYR   DS    CL3                                                              
*                                                                               
NTMPDT   DS    0CL8                                                             
NTMPMON  DS    CL3                                                              
NTMPDAY  DS    CL2                                                              
NTMPYR   DS    CL3                                                              
DCTMPDT  DS    CL6                                                              
* -------------------------------------------------------------------           
         SPACE 2                                                                
         EJECT                                                                  
       ++INCLUDE DDPSTBLK                                                       
         EJECT                                                                  
       ++INCLUDE SPGENMSR                                                       
         EJECT                                                                  
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE DDGLPFMD                                                       
* PRINT OFF = DDSPOOLD,DDCOMFACS,DDFLDIND,FATIOB,SPGENCLT                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE FATIOB                                                         
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         PRINT ON                                                               
* ---------------------------------------------------                           
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
         DS    CL2                                                              
LSTCLT   DS    CL3                                                              
         DS    CL4                                                              
LSTPRD   DS    CL2                                                              
         DS    CL5                                                              
LSTEST   DS    CL2                                                              
         DS    CL5                                                              
LSTMKT   DS    CL4                                                              
         DS    CL5                                                              
LSTSTA   DS    CL5                                                              
         DS    CL6                                                              
LSTMOS   DS    CL6                                                              
         DS    CL8                                                              
LSTSTAT  DS    CL1                                                              
* -------------------------------------------------------                       
DELINED  DSECT                      FOR MULTIPLE FIELDS OF MAINT SCREEN         
         DS    CL9                                                              
DLINVNUM DS    CL10                                                             
         DS    CL9                                                              
DLINVDT  DS    CL8                                                              
         DS    CL9                                                              
DLADDDT  DS    CL8                                                              
         DS    CL6                                                              
DLINVAMT DS    CL14                                                             
         DS    CL2                                                              
DLPAID   DS    C                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'028SPSFM47   03/14/02'                                      
         END                                                                    
