*          DATA SET SPSFM03    AT LEVEL 026 AS OF 06/09/04                      
*PHASE T21703A                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        T21703  -- AUTOPAY CLEARANCE MAINTENANCE & LIST      *         
*                                                                     *         
*  COMMENTS:     MAINTAINS AUTOPAY CLEARANCE RECORDS                  *         
*                                                                     *         
*  CALLED FROM:  SFM CONTROLLER (T21700), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       SCREEN SCSFMED (MAINT) & SCSFM7B (LIST)              *         
*                                                                     *         
*  OUTPUTS:                                                           *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- SCREEN FIELD HEADER                            *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- WORK                                           *         
*                R6 -- GETEL REGISTER                                 *         
*                R7 -- SECOND BASE                                    *         
*                R8 -- SPOOL                                          *         
*                R9 -- SYSD                                           *         
*                RA -- TWA                                            *         
*                RB -- FIRST BASE                                     *         
*                RC -- GEND                                           *         
*                RD -- SYSTEM                                         *         
*                RE -- SYSTEM                                         *         
*                RF -- SYSTEM                                         *         
*                                                                     *         
***********************************************************************         
         TITLE 'T21703 - AUTOPAY CLEARANCE MAINTENANCE AND LIST'                
T21703   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1703**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
*                                                                               
         BAS   RE,SETUP                                                         
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY (FOR LIST)                       
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS                                                    
         BE    LR                  LIST RECORDS                                 
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*                       VALIDATE KEY                                  *         
***********************************************************************         
VK       DS    0X                                                               
         BAS   RE,CLRNAME          CLEAR NAME FIELDS                            
*                                                                               
         XC    KEYAGYMD,KEYAGYMD   PARTS OF THE KEY                             
         XC    KEYCLT,KEYCLT                                                    
         XC    KEYPRD,KEYPRD                                                    
         XC    KEYPTR,KEYPTR                                                    
         XC    KEYEST,KEYEST                                                    
         XC    KEYSTA,KEYSTA                                                    
         XC    KEYDATE,KEYDATE                                                  
*                                                                               
         B     VK20                                                             
VK10     LA    R2,MNTMEDH                                                       
         MVI   8(R2),C'T'          STICK IN A "T" FOR MEDIA, IF BLANK           
         MVI   5(R2),1                                                          
         OI    6(R2),X'80'                                                      
*                                                                               
VK20     LA    R2,MNTMEDH                                                       
         CLI   5(R2),0                                                          
         BE    ERRMIS              REQUIRED FOR MAINT.                          
         MVI   USEIONUM,2                                                       
         GOTO1 VALIMED                                                          
         MVC   KEYAGYMD,BAGYMD                                                  
         MVC   MNTMDN,MEDNM                                                     
         OI    MNTMDNH+6,X'80'                                                  
*                                                                               
         LA    R2,MNTCLTH                                                       
         CLI   5(R2),0                                                          
         BNE   *+16                                                             
         CLI   ACTNUM,ACTLIST      LIST?                                        
         BE    VK30                                                             
         B     ERRMIS              REQUIRED FOR MAINT.                          
         MVI   USEIONUM,2                                                       
         GOTO1 VALICLT                                                          
         MVC   KEYCLT,BCLT                                                      
         MVC   MNTCLN,CLTNM                                                     
         OI    MNTCLNH+6,X'80'                                                  
*                                                                               
VK30     LA    R2,MNTPRDH                                                       
         CLI   5(R2),0                                                          
         BNE   *+16                                                             
         CLI   ACTNUM,ACTLIST      LIST?                                        
         BE    VK35                                                             
         B     ERRMIS              REQUIRED FOR MAINT.                          
         MVI   USEIONUM,2                                                       
         GOTO1 VALIPRD                                                          
         MVC   KEYPRD,BPRD                                                      
         MVC   MNTPDN,PRDNM                                                     
         OI    MNTPDNH+6,X'80'                                                  
*                                                                               
VK35     LA    R2,MNTPTRH                                                       
         CLI   5(R2),0                                                          
         BE    VK40                                                             
         MVI   USEIONUM,2                                                       
         GOTO1 VALIPRD                                                          
         MVC   KEYPTR,BPRD                                                      
         MVC   MNTPRN,PRDNM                                                     
         OI    MNTPRNH+6,X'80'                                                  
*                                                                               
VK40     LA    R2,MNTESTH                                                       
         CLI   5(R2),0                                                          
         BNE   *+16                                                             
         CLI   ACTNUM,ACTLIST      LIST?                                        
         BE    VK50                                                             
         B     ERRMIS              REQUIRED FOR MAINT.                          
         XC    BEST,BEST                                                        
         CLC   =C'000',8(R2)                                                    
         BE    VK50                                                             
         MVI   USEIONUM,2                                                       
         GOTO1 VALIEST                                                          
         MVC   KEYEST,BEST                                                      
         L     R6,AIO                                                           
         USING ESTHDR,R6                                                        
         GOTO1 DATCON,DMCB,(0,ESTART),(10,MNTESD)                               
         GOTO1 DATCON,DMCB,(0,EEND),(10,MNTESD+9)                               
         MVI   MNTESD+8,C'-'                                                    
         OI    MNTESDH+6,X'80'                                                  
         DROP  R6                                                               
*                                                                               
VK50     LA    R2,MNTSTAH                                                       
         CLI   5(R2),0                                                          
         BNE   *+16                                                             
         CLI   ACTNUM,ACTLIST      LIST?                                        
         BE    VK60                                                             
         B     ERRMIS              REQUIRED FOR MAINT.                          
         GOTO1 VALISTA                                                          
         MVC   KEYSTA,BSTA                                                      
         MVC   MNTMKN,MKTNM                                                     
         OI    MNTMKNH+6,X'80'                                                  
*                                                                               
VK60     LA    R2,MNTDATH                                                       
         CLI   5(R2),0                                                          
         BNE   *+16                                                             
         CLI   ACTNUM,ACTLIST      LIST?                                        
         BE    VK80                                                             
         B     ERRMIS              REQUIRED FOR MAINT.                          
         GOTO1 DATVAL,DMCB,(0,8(R2)),DATETEMP                                   
         CLC   DATETEMP,=C'000000'                                              
         BE    ERRINV                                                           
         GOTO1 DATCON,DMCB,(0,DATETEMP),(2,KEYDATE)                             
         XC    KEYDATE,=X'FFFF'       COMPLMENT THE DATE                        
*                                                                               
VK80     XC    KEY,KEY             PREPARE KEY                                  
         LA    R1,KEY                                                           
         USING APYKEY,R1                                                        
*                                                                               
         MVI   APYKTYP,APYKTYPQ    X'0D'                                        
         MVI   APYKSUB,APYKSUBQ    X'3A'                                        
         MVC   APYKDATE,KEYDATE                                                 
         OC    APYKDATE,APYKDATE   IS THERE A DATE                              
         BZ    *+10                NO THEN DON'T BOTHER FILLING IN A/M          
         MVC   APYKAGMD,KEYAGYMD                                                
         MVC   APYKCLT,KEYCLT                                                   
         MVC   APYKPRD,KEYPRD                                                   
         MVC   APYKPTN,KEYPTR                                                   
         MVC   APYKEST,KEYEST                                                   
         MVC   APYKSTA,KEYSTA                                                   
         DROP  R1                                                               
*                                                                               
         XC    SAVEKEY,SAVEKEY     PREPARE A COPY OF KEY                        
         MVC   SAVEKEY,KEY                                                      
         MVC   AIO,AIO1                                                         
*                                                                               
VKX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                       VALIDATE RECORD                               *         
***********************************************************************         
VR       DS    0X                                                               
         MVI   ELCODE,APYELQ                                                    
         GOTO1 REMELEM                                                          
         MVI   ELCODE,APYIELQ                                                   
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R3,SAVEKEY                                                       
         USING APYRECD,R3                                                       
*                                                                               
         LA    R4,ELEM                                                          
         USING APYEL,R4                                                         
         XC    ELEM,ELEM                                                        
         MVI   APYEL,APYELQ        ALPHA ELEMENT CODE                           
         MVI   APYLEN,APYLENQ      ELEMENT LENGTH                               
*                                                                               
         LA    R2,MNTDMEH          MEDIA DATA                                   
         CLI   MNTMEDH+5,0                                                      
         BE    ERRMIS                                                           
         MVI   USEIONUM,2                                                       
         GOTO1 VALIMED                                                          
         CLC   APYKAGMD,BAGYMD                                                  
         BNE   ERRCHA              CAN'T CHANGE DATA                            
         MVC   APYMED,8(R2)                                                     
*                                                                               
         LA    R2,MNTDCLH          CLIENT DATA                                  
         CLI   MNTCLTH+5,0                                                      
         BE    ERRMIS                                                           
         MVI   USEIONUM,2                                                       
         GOTO1 VALICLT                                                          
         CLC   APYKCLT,BCLT                                                     
         BNE   ERRCHA                                                           
         MVC   APYCLT,8(R2)                                                     
*                                                                               
         LA    R2,MNTDPRH          PRODUCT DATA                                 
         CLI   MNTPRDH+5,0                                                      
         BE    ERRMIS                                                           
         MVI   USEIONUM,2                                                       
         GOTO1 VALIPRD                                                          
         CLC   APYKPRD,BPRD                                                     
         BNE   ERRCHA                                                           
         MVC   APYPRD,8(R2)                                                     
*                                                                               
         LA    R2,MNTDP2H           PARTNER DATA                                
         CLI   MNTDP2H+5,0                                                      
         BE    VR10                                                             
         MVI   USEIONUM,2                                                       
         GOTO1 VALIPRD                                                          
         MVC   APYPRD2,8(R2)                                                    
*                                                                               
VR10     LA    R2,MNTDESH          ESTIMATE DATA                                
         CLI   MNTESTH+5,0                                                      
         BE    ERRMIS                                                           
         XC    BEST,BEST                                                        
         CLC   =C'000',8(R2)                                                    
         BE    VR12                                                             
         MVI   USEIONUM,2                                                       
         GOTO1 VALIEST                                                          
VR12     CLC   APYKEST,BEST                                                     
         BNE   ERRCHA                                                           
         MVC   APYEST,8(R2)                                                     
*                                                                               
         LA    R2,MNTDSTH          STATION DATA                                 
         CLI   MNTDSTH+5,0                                                      
         BE    ERRMIS                                                           
         MVI   USEIONUM,2                                                       
         GOTO1 VALISTA                                                          
         CLC   APYKSTA,BSTA                                                     
         BNE   ERRCHA                                                           
         MVC   APYSTA,8(R2)                                                     
*                                                                               
         LA    R2,MNTMKTH          MARKET DATA                                  
         CLI   MNTMKTH+5,0                                                      
         BE    ERRMIS                                                           
         MVI   USEIONUM,2                                                       
         GOTO1 VALIMKT                                                          
         MVC   APYMKT,QMKT                                                      
*                                                                               
         LA    R2,MNTDMOH          MONTH YEAR DATA (MMM/YY)                     
         CLI   MNTDATH+5,0                                                      
         BE    ERRMIS                                                           
         GOTO1 DATVAL,DMCB,(2,8(R2)),DATETEMP                                   
         CLC   DATETEMP,=C'000000'                                              
         BE    ERRINV                                                           
*                                                                               
         GOTO1 DATCON,DMCB,DATETEMP,(14,APYMONTH)                               
         DROP  R3                                                               
*                                                                               
         LA    R2,MNTSREP          SPECIAL REP                                  
         CLI   5(R2),0                                                          
         BE    VR60                OPTIONAL FIELD                               
         MVC   APYSREP,MNTSREP                                                  
*                                                                               
VR60     LA    R2,MNTPAYH          PAYER/REQUESTOR                              
         CLI   5(R2),0                                                          
         BE    VR70                OPTIONAL FIELD                               
         MVC   APYPAYER,MNTPAY                                                  
*                                                                               
VR70     LA    R2,MNTPDTH          PAID DATE                                    
         CLI   5(R2),0                                                          
         BE    VR80                OPTIONAL FIELD                               
         GOTO1 DATVAL,DMCB,(2,8(R2)),DATETEMP                                   
         CLC   DATETEMP,=C'000000'                                              
         BE    ERRINV                                                           
         GOTO1 DATCON,DMCB,DATETEMP,(2,APYPAID)                                 
*                                                                               
VR80     MVC   AIO,AIO1                                                         
         XC    KEY,KEY                                                          
         MVC   KEY(L'SAVEKEY),SAVEKEY                                           
         GOTO1 ADDELEM                                                          
*                                                                               
         LA    R2,MNTINVH          INVOICE LIST                                 
         CLI   MNTINVH+5,0                                                      
         BE    ERRMIS                                                           
         GOTO1 SCANNER,DMCB,MNTINVH,(10,BLOCK),=C',,'                           
         CLI   DMCB+4,0                                                         
         BNE   *+6                                                              
         DC    H'0'                                                             
         LA    R5,BLOCK                                                         
         USING SCANBLKD,R5                                                      
*                                                                               
         LA    R4,ELEM                                                          
         USING APYIEL,R4                                                        
         XC    ELEM,ELEM                                                        
         MVI   APYIEL,APYIELQ      ALPHA ELEMENT CODE                           
         MVI   APYILEN,APYIHLNQ    ELEMENT HDR LEN                              
*                                                                               
         SR    R1,R1               COUNT INVOICES                               
         LA    R2,APYIINV                                                       
VR90     MVC   0(L'APYIINV,R2),SC1STFLD                                         
         LA    R1,1(R1)                                                         
         LA    R2,L'APYIINV(R2)                                                 
         LA    R5,SCBLKLQ(R5)                                                   
         CLI   SC1STLEN,0                                                       
         BNE   VR90                                                             
         STC   R1,APYINUM          NUMBER OF INVOICES                           
         MHI   R1,APYILENQ         LENGTH OF EACH INVOICE*NUM                   
         AHI   R1,APYIHLNQ         ADD IN LEN OF HDR                            
         STC   R1,APYILEN                                                       
         GOTO1 ADDELEM                                                          
         DROP  R4,R5                                                            
*                                                                               
VRX      B     DR                  REDISPLAY RECORD                             
         EJECT                                                                  
***********************************************************************         
*                       DISPLAY RECORD                                *         
***********************************************************************         
DR       DS    0X                                                               
         BAS   RE,CLRDATA          CLEAR DATA FIELDS                            
*                                                                               
         L     R6,AIO                                                           
         USING APYEL,R6                                                         
         MVI   ELCODE,APYELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   DRX                 ELEMENT ALWAYS PRESENT                       
*                                                                               
         MVC   MNTDME,APYMED       DISPLAY DATA IN RECORD                       
         MVC   MNTDCL,APYCLT                                                    
         MVC   MNTDPR,APYPRD                                                    
         MVC   MNTDP2,APYPRD2                                                   
         MVC   MNTDES,APYEST                                                    
         MVC   MNTDST,APYSTA                                                    
         MVC   MNTMKT,APYMKT                                                    
         MVC   MNTDMO,APYMONTH                                                  
         MVC   MNTSREP,APYSREP                                                  
*                                                                               
         CLI   APYLEN,APYLN2Q      BIG ELEM                                     
         BL    *+20                                                             
         CLC   APYACN,SPACES                                                    
         BNH   *+10                                                             
         MVC   MNTSREP,APYACN      ACN NUMBER FOR COKE                          
*                                                                               
         MVC   MNTPAY,APYPAYER                                                  
         GOTO1 DATCON,DMCB,(2,APYPAID),(11,MNTPDT)                              
         GOTO1 HEXOUT,DMCB,APYERRS,MNTERR,1,=C'TOG'                             
*                                                                               
         BAS   RE,NEXTEL                                                        
         BNE   DR20                                                             
         MVC   MNTDME2,APYMED       DISPLAY DATA IN RECORD                      
         MVC   MNTDCL2,APYCLT                                                   
         MVC   MNTDPR2,APYPRD                                                   
         MVC   MNTDP22,APYPRD2                                                  
         MVC   MNTDES2,APYEST                                                   
         MVC   MNTDST2,APYSTA                                                   
         MVC   MNTMKT2,APYMKT                                                   
         MVC   MNTDMO2,APYMONTH                                                 
         MVC   MNTSRE2,APYSREP                                                  
*                                                                               
         CLI   APYLEN,APYLN2Q      BIG ELEM                                     
         BL    *+20                                                             
         CLC   APYACN,SPACES                                                    
         BNH   *+10                                                             
         MVC   MNTSRE2,APYACN      ACN NUMBER FOR COKE                          
*                                                                               
         MVC   MNTPAY2,APYPAYER                                                 
         GOTO1 DATCON,DMCB,(2,APYPAID),(11,MNTPDT2)                             
         GOTO1 HEXOUT,DMCB,APYERRS,MNTERR2,1,=C'TOG'                            
*                                                                               
         BAS   RE,NEXTEL                                                        
         BNE   DR20                                                             
         MVC   MNTDME3,APYMED       DISPLAY DATA IN RECORD                      
         MVC   MNTDCL3,APYCLT                                                   
         MVC   MNTDPR3,APYPRD                                                   
         MVC   MNTDP23,APYPRD2                                                  
         MVC   MNTDES3,APYEST                                                   
         MVC   MNTDST3,APYSTA                                                   
         MVC   MNTMKT3,APYMKT                                                   
         MVC   MNTDMO3,APYMONTH                                                 
         MVC   MNTSRE3,APYSREP                                                  
*                                                                               
         CLI   APYLEN,APYLN2Q      BIG ELEM                                     
         BL    *+20                                                             
         CLC   APYACN,SPACES                                                    
         BNH   *+10                                                             
         MVC   MNTSRE3,APYACN      ACN NUMBER FOR COKE                          
*                                                                               
         MVC   MNTPAY3,APYPAYER                                                 
         GOTO1 DATCON,DMCB,(2,APYPAID),(11,MNTPDT3)                             
         GOTO1 HEXOUT,DMCB,APYERRS,MNTERR3,1,=C'TOG'                            
*                                                                               
         DROP  R6                                                               
*                                                                               
DR20     L     R6,AIO                                                           
         USING APYIEL,R6                                                        
         MVI   ELCODE,APYIELQ      X'02'                                        
         BAS   RE,GETEL                                                         
         BNE   DR90                ELEMENT ALWAYS PRESENT                       
         XC    BLOCK(100),BLOCK                                                 
         ZIC   R1,APYINUM          NUMBER OF INVOICES                           
         LA    R3,APYIINV          FIRST INVOICE                                
         LA    R2,BLOCK            SQUASH IN BLOCK                              
DR22     MVC   0(L'APYIINV,R2),0(R3)                                            
         SH    R1,=H'1'                                                         
         BZ    DR25                                                             
         MVI   10(R2),C','                                                      
         LA    R2,11(R2)                                                        
         AHI   R3,APYILENQ         NEXT INV                                     
         B     DR22                                                             
DR25     GOTO1 SQUASHER,DMCB,BLOCK,100                                          
         MVC   MNTINV,BLOCK                                                     
         DROP  R6                                                               
*                                                                               
DR90     CLI   ACTNUM,ACTCHA       SAVED ACTION = CHANGE?                       
         BE    DR100                                                            
         CLI   ACTNUM,ACTSEL       SAVED ACTION = SEL?                          
         BNE   DRX                                                              
         CLI   LISTSEL,C'C'        SEL ACTION = CHANGE?                         
         BNE   DRX                                                              
*                                                                               
DR100    MVC   KEY(L'SAVEKEY),SAVEKEY                                           
         MVC   AIO,AIO2                                                         
         GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
DRX      MVC   AIO,AIO1                                                         
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                       DISPLAY KEY                                   *         
***********************************************************************         
DK       DS    0X                                                               
*                                                                               
         XC    KEYAGYMD,KEYAGYMD   PARTS OF THE KEY                             
         XC    KEYCLT,KEYCLT                                                    
         XC    KEYPRD,KEYPRD                                                    
         XC    KEYPTR,KEYPTR                                                    
         XC    KEYEST,KEYEST                                                    
         XC    KEYSTA,KEYSTA                                                    
         XC    KEYDATE,KEYDATE                                                  
*                                                                               
         L     R6,AIO                                                           
         USING APYRECD,R6                                                       
*                                                                               
         MVC   MNTMED,APYKAGMD     CONVERT BAGYMD INTO 1 CHAR MEDIA             
         NI    MNTMED,X'0F'        TURN OFF AGENCY PORTION                      
         LA    R5,MEDTAB                                                        
*                                                                               
DK30     CLC   MNTMED,1(R5)                                                     
         BE    DK40                                                             
         LA    R5,MEDTABLQ(R5)     NEXT ITEM IN MEDIA TABLE                     
         CLI   1(R5),X'FF'         END OF TABLE?                                
         BNE   DK30                                                             
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(37),=C'ERROR - END OF MEDIA TABLE IS REACHED'            
         OI    CONHEADH+6,X'80'                                                 
         GOTO1 ERREX2              HOPE THIS WON'T HAPPEN...                    
DK40     MVC   MNTMED,0(R5)        DISPLAY 1 CHAR MEDIA                         
         OI    MNTMEDH+6,X'80'                                                  
         MVI   MNTMEDH+5,1         SET INPUT LENGTH MANUALLY                    
         LA    R2,MNTMEDH                                                       
         MVI   USEIONUM,2                                                       
         GOTO1 VALIMED                                                          
         MVC   KEYAGYMD,BAGYMD                                                  
         MVC   MNTMDN,MEDNM        DISPLAY MEDIA NAME                           
         OI    MNTMDNH+6,X'80'                                                  
*                                                                               
         XC    MNTCLT,MNTCLT       CLIENT                                       
         GOTO1 CLUNPK,DMCB,APYKCLT,MNTCLT                                       
         OI    MNTCLTH+6,X'80'                                                  
         MVI   MNTCLTH+5,3                                                      
         CLI   MNTCLT+2,X'00'                                                   
         BE    *+12                                                             
         CLI   MNTCLT+2,C' '                                                    
         BNE   *+8                                                              
         MVI   MNTCLTH+5,2                                                      
         LA    R2,MNTCLTH                                                       
         MVI   USEIONUM,2                                                       
         GOTO1 VALICLT                                                          
         MVC   KEYCLT,BCLT                                                      
         MVC   MNTCLN,CLTNM        DISPLAY CLIENT NAME                          
         OI    MNTCLNH+6,X'80'                                                  
*                                                                               
         XC    MNTPRD,MNTPRD       PRODUCT                                      
         L     R2,AIO                                                           
         USING CLTHDR,R2                                                        
         LA    R5,CLIST                                                         
DK50     CLC   APYKPRD,3(R5)       COMPARE PRODUCT NUMBER                       
         BE    *+12                                                             
         LA    R5,4(R5)            BUMP TO NEXT PRODUCT                         
         B     DK50                                                             
         MVC   MNTPRD,0(R5)        GET PRODUCT MNMONIC                          
         OI    MNTPRDH+6,X'80'                                                  
         DROP  R2                                                               
         MVI   MNTPRDH+5,3                                                      
         CLI   MNTPRD+2,X'00'                                                   
         BE    *+12                                                             
         CLI   MNTPRD+2,C' '                                                    
         BNE   *+8                                                              
         MVI   MNTPRDH+5,2                                                      
         LA    R2,MNTPRDH                                                       
         MVI   USEIONUM,2                                                       
         GOTO1 VALIPRD                                                          
         MVC   KEYPRD,BPRD                                                      
         MVC   MNTPDN,PRDNM        DISPLAY PRODUCT NAME                         
         OI    MNTPDNH+6,X'80'                                                  
*                                                                               
         XC    MNTPTR,MNTPTR       PARTNER                                      
         CLI   APYKPTN,0                                                        
         BE    DK60                                                             
         L     R2,AIO                                                           
         USING CLTHDR,R2                                                        
         LA    R5,CLIST                                                         
DK55     CLC   APYKPTN,3(R5)       COMPARE PRODUCT NUMBER                       
         BE    *+12                                                             
         LA    R5,4(R5)            BUMP TO NEXT PRODUCT                         
         B     DK55                                                             
         MVC   MNTPTR,0(R5)        GET PRODUCT MNMONIC                          
         OI    MNTPTRH+6,X'80'                                                  
         DROP  R2                                                               
         MVI   MNTPTRH+5,3                                                      
         CLI   MNTPTR+2,X'00'                                                   
         BE    *+12                                                             
         CLI   MNTPTR+2,C' '                                                    
         BNE   *+8                                                              
         MVI   MNTPTRH+5,2                                                      
         LA    R2,MNTPTRH                                                       
         MVI   USEIONUM,2                                                       
         GOTO1 VALIPRD                                                          
         MVC   KEYPTR,BPRD                                                      
         MVC   MNTPRN,PRDNM        DISPLAY PRODUCT NAME                         
         OI    MNTPRNH+6,X'80'                                                  
*                                                                               
DK60     XC    MNTEST,MNTEST       ESTIMATE                                     
         EDIT  APYKEST,(L'MNTEST,MNTEST),FILL=0                                 
         MVI   MNTESTH+5,3                                                      
         OI    MNTESTH+6,X'80'                                                  
         MVI   MNTESTH+4,X'08'     VALID NUMERIC                                
         LA    R2,MNTESTH                                                       
         XC    BEST,BEST                                                        
         CLC   =C'000',8(R2)                                                    
         BE    DK62                                                             
         MVI   USEIONUM,2                                                       
         GOTO1 VALIEST                                                          
DK62     MVC   KEYEST,BEST                                                      
         L     R2,AIO                                                           
         USING ESTHDR,R2                                                        
         GOTO1 DATCON,DMCB,(0,ESTART),(10,MNTESD)                               
         GOTO1 DATCON,DMCB,(0,EEND),(10,MNTESD+9)                               
         MVI   MNTESD+8,C'-'                                                    
         OI    MNTESDH+6,X'80'                                                  
         DROP  R2                                                               
*                                                                               
         XC    BMKTSTA,BMKTSTA     STATION                                      
         MVC   BMKTSTA(L'SVBMKT),SVBMKT                                         
         MVC   BMKTSTA+L'SVBMKT(L'APYKSTA),APYKSTA                              
         GOTO1 MSUNPK,DMCB,BMKTSTA,FULL,MNTSTA                                  
         OI    MNTSTAH+6,X'80'                                                  
         MVI   MNTSTAH+5,5                                                      
         LA    R2,MNTSTAH                                                       
         MVI   USEIONUM,2                                                       
         GOTO1 VALISTA                                                          
         MVC   KEYSTA,BSTA                                                      
         MVC   MNTMKN,MKTNM                                                     
         OI    MNTMKNH+6,X'80'                                                  
*                                                                               
         MVC   HALF,APYKDATE       DATE                                         
         XC    HALF,=X'FFFF'       UNCOMPLMENT THE DATE                         
         GOTO1 DATCON,DMCB,(2,HALF),(10,MNTDAT)                                 
         MVC   KEYDATE,APYKDATE                                                 
         OI    MNTDATH+6,X'80'                                                  
         DROP  R6                                                               
*                                                                               
         MVC   LISTSEL,THISLSEL    SELECTED ACTION ON LIST                      
*                                                                               
         XC    KEY,KEY             PREPARE KEY                                  
         LA    R1,KEY                                                           
         USING APYRECD,R1                                                       
         MVI   APYKTYP,APYKTYPQ    X'0D'                                        
         MVI   APYKSUB,APYKSUBQ    X'3A'                                        
         MVC   APYKDATE,KEYDATE                                                 
         MVC   APYKAGMD,KEYAGYMD                                                
         MVC   APYKCLT,KEYCLT                                                   
         MVC   APYKPRD,KEYPRD                                                   
         MVC   APYKPTN,KEYPTR                                                   
         MVC   APYKEST,KEYEST                                                   
         MVC   APYKSTA,KEYSTA                                                   
         DROP  R1                                                               
*                                                                               
         XC    SAVEKEY,SAVEKEY     PREPARE A COPY OF KEY                        
         MVC   SAVEKEY,KEY                                                      
         MVC   AIO,AIO1                                                         
DKX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                       LIST RECORDS                                  *         
***********************************************************************         
LR       DS    0X                                                               
*                                                                               
         LA    R6,KEY                                                           
         USING APYRECD,R6                                                       
         OC    KEY,KEY             FIRST TIME THROUGHT?                         
         BNZ   LR10                                                             
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'SAVEKEY),SAVEKEY                                           
*                                                                               
LR10     GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         BE    LR30                                                             
         DC    H'0'                                                             
*                                                                               
LR20     LA    R6,KEY                                                           
         GOTO1 SEQ                                                              
         CLI   DMCB+8,0                                                         
         BE    LR30                                                             
         DC    H'0'                                                             
*                                                                               
LR30     CLI   APYKTYP,APYKTYPQ    SAME SYSTEM/PROGRAM?    X'0D'                
         BNE   LRX                                                              
         CLI   APYKSUB,APYKSUBQ                            X'3A'                
         BNE   LRX                                                              
         CLC   APYKAGMD,KEYAGYMD                                                
         BNE   LR20                                                             
         OC    KEYDATE,KEYDATE     ANYTHING IN DATE (FILTER)?                   
         BZ    *+14                                                             
         CLC   APYKDATE,KEYDATE                                                 
         BNE   LR20                                                             
         OC    KEYCLT,KEYCLT       ANYTHING IN CLIENT (FILTER)?                 
         BZ    *+14                                                             
         CLC   APYKCLT,KEYCLT                                                   
         BNE   LR20                                                             
         OC    KEYPRD,KEYPRD       ANYTHING IN PRODUCT (FILTER)?                
         BZ    *+14                                                             
         CLC   APYKPRD,KEYPRD                                                   
         BNE   LR20                                                             
         CLC   =C'000',MNTEST                                                   
         BE    *+14                                                             
         OC    KEYEST,KEYEST       ANYTHING IN ESTIMATE (FILTER)?               
         BZ    *+14                                                             
         CLC   APYKEST,KEYEST                                                   
         BNE   LR20                                                             
         OC    KEYSTA,KEYSTA       ANYTHING IN STATION (STARTING PT)?           
         BZ    *+14                                                             
         CLC   APYKSTA,KEYSTA                                                   
         BL    LR20                                                             
         DROP  R6                                                               
*                                                                               
         XC    LRKEY,LRKEY         COPY OF KEY FOR RECORD RESTORATION           
         MVC   LRKEY,KEY                                                        
         MVC   LISTAR,SPACES                                                    
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO                                                           
         USING APYEL,R6                                                         
         MVI   ELCODE,APYELQ                                                    
         BAS   RE,GETEL                                                         
         BE    LR50                ELEMENT ALWAYS PRESENT                       
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(28),=C'ERROR - ALPHA ELEM NOT FOUND'                     
         OI    CONHEADH+6,X'80'                                                 
         GOTO1 ERREX2              HOPE THIS WON'T HAPPEN...                    
*                                                                               
LR50     MVC   LSMOY,APYMONTH                                                   
*                                                                               
         MVC   LSSREP,APYSREP                                                   
         CLC   AGENCY,=C'CK'       COKE                                         
         BNE   *+10                                                             
         MVC   LSSREP,APYMKT       DISPLAY MARKET                               
*                                                                               
         CLI   APYERRS,0                                                        
         BE    LR52                                                             
         GOTO1 HEXOUT,DMCB,APYERRS,LSERR,1,=C'TOG'                              
*                                                                               
LR52     OC    APYPAID,APYPAID     ANY PAID DATE                                
         BZ    LR55                                                             
         GOTO1 DATCON,DMCB,(2,APYPAID),(11,LSPAYDT)                             
         DROP  R6                                                               
*                                                                               
LR55     L     R6,AIO                                                           
         USING APYRECD,R6                                                       
*                                                                               
         TM    APYRCNTL,APYRCMON   MULTIPE MONTHS                               
         BNO   *+8                                                              
         MVI   LSERR,C'*'                                                       
*                                                                               
         MVC   LSMED,APYKAGMD      CONVERT BAGYMD INTO 1 CHAR MEDIA             
         NI    LSMED,X'0F'         TURN OFF AGENCY PORTION                      
         LA    R5,MEDTAB                                                        
*                                                                               
LR60     CLC   LSMED,1(R5)                                                      
         BE    LR70                                                             
         LA    R5,MEDTABLQ(R5)     NEXT ITEM IN MEDIA TABLE                     
         CLI   1(R5),X'FF'         END OF TABLE?                                
         BNE   LR60                                                             
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(37),=C'ERROR - END OF MEDIA TABLE IS REACHED'            
         OI    CONHEADH+6,X'80'                                                 
         GOTO1 ERREX2              HOPE THIS WON'T HAPPEN...                    
LR70     MVC   LSMED,0(R5)         PUT 1 CHAR MEDIA INTO LIST LINE              
*                                                                               
         GOTO1 CLUNPK,DMCB,APYKCLT,LSCLT                                        
*                                                                               
         XC    FAKEFLD,FAKEFLD                                                  
         MVI   FAKEFLD,X'0B'       MAX LENGTH = L'HEADER + L'DATA               
         MVI   FAKEFLD+5,3         LENGTH OF DATA                               
         MVC   FAKEFLD+8(L'LSCLT),LSCLT                                         
*                                                                               
         LA    R2,FAKEFLD                                                       
         MVI   USEIONUM,2                                                       
         GOTO1 VALICLT                                                          
*                                                                               
         L     R2,AIO              FOLLOWING SEARCH CAN'T NOT FAIL!!            
         USING CLTHDR,R2                                                        
         LA    R5,CLIST                                                         
*                                                                               
LR80     CLC   APYKPRD,3(R5)       COMPARE PRODUCT NUMBER                       
         BE    *+12                                                             
         LA    R5,4(R5)            BUMP TO NEXT PRODUCT                         
         B     LR80                                                             
         MVC   LSPRD,0(R5)         GET PRODUCT MNMONIC                          
         DROP  R2                                                               
*                                                                               
         MVC   LSPRD2,APYPRD2                                                   
*                                                                               
         CLC   AGENCY,=C'CK'       COKE                                         
         BNE   LR85                                                             
         CLI   APYLEN,APYLN2Q      BIG ELEM                                     
         BL    LR85                                                             
         CLC   APYACN,SPACES                                                    
         BNH   LR85                                                             
         MVI   LSPRD2,C'*'         INDICATE ACN ON LIST SCREEN                  
*                                                                               
LR85     EDIT  APYKEST,(L'LSEST,LSEST),FILL=0                                   
*                                                                               
         MVC   HALF,APYKDATE                                                    
         XC    HALF,=X'FFFF'       UNCOMPLIMENT                                 
         GOTO1 DATCON,DMCB,(2,HALF),(10,LSDATE)                                 
*                                                                               
         XC    BMKTSTA,BMKTSTA                                                  
         MVC   BMKTSTA(L'SVBMKT),SVBMKT                                         
         MVC   BMKTSTA+L'SVBMKT(L'APYKSTA),APYKSTA                              
         GOTO1 MSUNPK,DMCB,BMKTSTA,FULL,LSSTA                                   
         DROP  R6                                                               
*                                                                               
         L     R6,AIO1                                                          
         USING APYIEL,R6                                                        
         MVI   ELCODE,APYIELQ      INVOICE ELEM                                 
         BAS   RE,GETEL                                                         
         BNE   LR140                                                            
         MVC   LSINV(L'APYIINV),APYIINV                                         
         CLI   APYINUM,1                                                        
         BNH   *+8                                                              
         MVI   LSINV+10,C'+'                                                    
         DROP  R6                                                               
*                                                                               
LR140    MVC   AIO,AIO1            RESTORE RECORD                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'LRKEY),LRKEY                                               
         GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC              NEEDED TO BUILT CORRECT DISK                 
         CLI   DMCB+8,0            ADDRESSES IN LISTMON TABLE                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
LR150    GOTO1 LISTMON                                                          
         B     LR20                                                             
*                                                                               
LRX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                          ERROR MESSAGES                             *         
***********************************************************************         
ERRCHA   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(40),=C'ERROR - DATA MUST BE SAME AS THAT OF KEY'         
         OI    CONHEADH+6,X'80'                                                 
         GOTO1 ERREX2                                                           
*                                                                               
ERRINV   MVI   ERROR,INVALID                                                    
         B     VSFMERR                                                          
ERRMIS   MVI   ERROR,MISSING                                                    
         B     VSFMERR                                                          
ERRBKLN  MVC   ERRNUM,=AL2(BKLNINV)                                             
         B     SPERREX                                                          
*                                                                               
SPERREX  OI    GENSTAT2,USGETTXT                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTMSGNO,ERRNUM                                                   
         MVI   GTMTYP,GTMERR                                                    
         MVI   GTMSYS,2                                                         
VSFMERR  MVC   AIO,AIO1                                                         
         GOTO1 ERREX                                                            
         DROP  RF                                                               
         SPACE 2                                                                
*                                  SHORT DESP OF ERROR MSGS                     
BKLNINV  EQU   449                 BREAK LN MUST BE 1-4                         
*                                                                               
         SPACE 2                                                                
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
*         CLEARS NAME FIELDS ON MAINTENANCE AND LIST SCREENS          *         
***********************************************************************         
CLRNAME  NTR1                                                                   
         CLI   MNTMEDH+5,0                                                      
         BNE   *+14                                                             
         XC    MNTMDN,MNTMDN       CLR MEDIA NAME                               
         OI    MNTMDNH+6,X'80'                                                  
         CLI   MNTCLTH+5,0                                                      
         BNE   *+14                                                             
         XC    MNTCLN,MNTCLN       CLR CLIENT NAME                              
         OI    MNTCLNH+6,X'80'                                                  
         CLI   MNTPRDH+5,0                                                      
         BNE   *+14                                                             
         XC    MNTPDN,MNTPDN       CLR PRODUCT NAME                             
         OI    MNTPDNH+6,X'80'                                                  
         CLI   MNTPTRH+5,0                                                      
         BNE   *+14                                                             
         XC    MNTPRN,MNTPRN       CLR PRODUCT NAME                             
         OI    MNTPRNH+6,X'80'                                                  
         CLI   MNTESTH+5,0                                                      
         BNE   *+14                                                             
         XC    MNTESD,MNTESD       CLR ESTIMATE START & END DATE                
         OI    MNTESDH+6,X'80'                                                  
         CLI   MNTSTAH+5,0                                                      
         BNE   *+14                                                             
         XC    MNTMKN,MNTMKN       CLR MARKET NAME                              
         OI    MNTMKNH+6,X'80'                                                  
*                                                                               
CLRNAMEX B     XIT                 END OF CLEAR NAME FIELDS                     
         EJECT                                                                  
***********************************************************************         
*         CLEARS DATA FIELDS ON MAINTENANCE SCREEN                    *         
***********************************************************************         
CLRDATA  NTR1                                                                   
         LA    R2,MNTDMEH                                                       
         LA    R3,MNTINVH                                                       
CLRD10   TM    1(R2),X'20'         PROTECTED                                    
         BO    CLRD20                                                           
         ZIC   R1,0(R2)                                                         
         AHI   R1,-9               8+1                                          
         BM    CLRD20                                                           
         EX    R1,*+4                                                           
         XC    8(0,R2),8(R2)                                                    
         OI    6(R2),X'80'                                                      
CLRD20   ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         CR    R2,R3                                                            
         BNH   CLRD10                                                           
CLRDATAX B     XIT                 END OF CLEAR DATA FIELDS                     
         EJECT                                                                  
***********************************************************************         
*        SETUP                                                        *         
***********************************************************************         
SETUP    NTR1                                                                   
         OI    GENSTAT1,USKYMRG+NOSETEFH                                        
         OI    CONSERVH+1,X'01'    MODIFY SERVICE REQUEST                       
         OI    CONSERVH+6,X'80'    TRANSMIT TO GET CONTROL                      
*                                                                               
*  ADD - IF THE MEDIA IS A AND IT'S A DDS TERMINAL ADD DUMMY RECORD             
*  X'0D3A' AND ALL NULLS FOR THE DIRECTORY TO COMPRESS AGAINST                  
*                                                                               
         CLI   TWAOFFC,C'*'        DDS TERM                                     
         BNE   SETUPX                                                           
         CLI   MODE,VALKEY         MAKE SURE IO AREAS ARE SET UP                
         BNE   SETUPX                                                           
         CLI   ACTEQU,ACTADD                                                    
         BNE   SETUPX                                                           
         CLI   MNTMED,C'A'                                                      
         BNE   SETUPX                                                           
         XC    KEY,KEY                                                          
         MVI   KEY,APYKTYPQ        X'0D'                                        
         MVI   KEY+1,APYKSUBQ      X'3A'                                        
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    SETUPX              RECORD ALREADY ON FILE                       
         MVC   KEY,KEYSAVE                                                      
*                                                                               
         XC    ELEM,ELEM                                                        
         MVI   ELEM,APYELQ         X'01'                                        
         MVI   ELEM+1,APYLENQ      ELEMENT LENGTH                               
         L     R6,AIO                                                           
         MVC   0(L'KEY,R6),KEY                                                  
         GOTO1 ADDELEM                                                          
         GOTO1 ADDREC                                                           
*                                                                               
SETUPX   B     XIT                                                              
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                        *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        TABLE FOR MEDIA AND ELEMENT CODES                            *         
***********************************************************************         
MEDTAB   DS    0X                                                               
         DC    CL1'T',XL1'01'                                                   
MEDTABLQ EQU   *-MEDTAB                                                         
         DC    CL1'R',XL1'02'                                                   
         DC    CL1'N',XL1'03'                                                   
         DC    CL1'X',XL1'04'                                                   
         DC    CL1'C',XL1'08'                                                   
         DC    X'FF'                                                            
*                                                                               
***********************************************************************         
*        TABLE FOR LOOKING UP ONE NIBBLE MONTH EQUIVALENTS            *         
***********************************************************************         
MONTAB   DS    0X                                                               
         DC    CL3'JAN',XL1'10'    FIRST NIBBLE SIGNIFIES MONTH                 
MONTABLQ EQU   *-MONTAB                                                         
         DC    CL3'FEB',XL1'20'    SECOND NIBBLE SIGNIFIES YEAR, WHICH          
         DC    CL3'MAR',XL1'30'    IS NOT PART OF THIS TABLE.                   
         DC    CL3'APR',XL1'40'                                                 
         DC    CL3'MAY',XL1'50'                                                 
         DC    CL3'JUN',XL1'60'                                                 
         DC    CL3'JUL',XL1'70'                                                 
         DC    CL3'AUG',XL1'80'                                                 
         DC    CL3'SEP',XL1'90'                                                 
         DC    CL3'OCT',XL1'A0'                                                 
         DC    CL3'NOV',XL1'B0'                                                 
         DC    CL3'DEC',XL1'C0'                                                 
         DC    X'FF'                                                            
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE SPSFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFMEDD          MAINTENACE SCREEN                            
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM7BD          LIST SCREEN                                  
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE SPGENAPY          AUTOPAY RECORD DSECT                         
         EJECT                                                                  
       ++INCLUDE SPGENEST          FOR ESTIMATE DATES                           
         EJECT                                                                  
       ++INCLUDE SPGENCLT          FOR PRODUCT MEMONIC                          
         EJECT                                                                  
       ++INCLUDE DDSCANBLKD        FOR SCANNER                                  
         EJECT                                                                  
       ++INCLUDE SPSFMWORKD                                                     
         EJECT                                                                  
* START OF SAVED STORAGE (6144)                                                 
*                                                                               
         ORG   SYSSPARE                                                         
RELO     DS    A                   RELOCATION FACTOR                            
LISTSEL  DS    C                   FOR USE WITH LIST SELECTIONS                 
SAVEKEY  DS    CL13                                                             
LRKEY    DS    CL13                LISR RECORD KEY                              
DATETEMP DS    CL6                 TEMP DATE FOR DATVAL                         
*                                                                               
KEYAGYMD DS    XL1                 WORKING STORAGE FOR KEY PARTS                
KEYCLT   DS    XL2                                                              
KEYPRD   DS    XL1                                                              
KEYPTR   DS    XL1                                                              
KEYEST   DS    XL1                                                              
KEYSTA   DS    XL3                                                              
KEYDATE  DS    XL2                                                              
KEYMOYR  DS    XL1                                                              
*                                                                               
MY6BYTE  DC    XL6'00'                                                          
SVBMKT   DC    XL2'00'             SAVE OFF BINARY MARKET FOR WORK              
FAKEFLD  DS    XL11                                                             
*                                                                               
ERRNUM   DS    XL2                                                              
SAVESEL  DS    CL1                                                              
         EJECT                                                                  
       ++INCLUDE FAGETTXTD         ERROR MSGS                                   
*                                                                               
         EJECT                                                                  
GEND     DSECT                                                                  
         ORG   LISTAR              LABELS FOR LISTMON                           
LSMED    DS    CL1                                                              
LSERR    DS    CL2                                                              
         DS    CL1                                                              
LSCLT    DS    CL3                                                              
         DS    CL1                                                              
LSPRD    DS    CL3                                                              
         DS    CL1                                                              
LSPRD2   DS    CL3                                                              
         DS    CL1                                                              
LSEST    DS    CL3                                                              
         DS    CL1                                                              
LSSTA    DS    CL8                                                              
         DS    CL1                                                              
LSMOY    DS    CL6                                                              
         DS    CL1                                                              
LSSREP   DS    CL4                                                              
         DS    CL2                                                              
LSPAYDT  DS    CL8                                                              
         DS    CL2                                                              
LSINV    DS    CL11                                                             
         DS    CL1                                                              
LSDATE   DS    CL8                                                              
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'026SPSFM03   06/09/04'                                      
         END                                                                    
