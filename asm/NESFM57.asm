*          DATA SET NESFM57    AT LEVEL 058 AS OF 08/23/16                      
*PHASE T31C57A                                                                  
***********************************************************************         
*  TITLE: T31C57 - WB FLIGHT                                                    
***********************************************************************         
         TITLE 'T31C57 WB FLIGHT ID'                                            
T31C57   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,WBFLT,R7                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         GOTO1 VTERMACC            CHECK FOR DISP/LIST ONLY TERMINALS           
*                                                                               
         MVI   IOOPT,C'Y'          CONTROL MY OWN ADDREC/PUTREC                 
         BAS   RE,SETXFIL          SET TO XSPDIR/XSPFIL                         
*                                                                               
         CLC   =C'DELETE',CONACT                                                
         BE    DELERR                                                           
*                                                                               
         CLI   T31CFFD+1,C'*'      DDS TERMINAL?                                
         BE    MAIN10                                                           
         CLC   =C'FLIGHTS',CONREC  COMING FROM SCRIPT?                          
         BE    MAIN10                                                           
         CLC   =C'OO',AGENCY       OMDUSEC CAN ADD OUTSIDE SCRIPT               
         BE    MAIN10                                                           
         CLC   =C'ADD',CONACT                                                   
         BE    INVLACT                                                          
*                                                                               
MAIN10   CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD KEY                          
         BE    VR                                                               
         CLI   MODE,DISPKEY        VALIDATE RECORD KEY                          
         BE    DK                                                               
         CLI   MODE,DISPREC        VALIDATE RECORD KEY                          
         BE    DR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,PRINTREP                                                    
         BE    PR                                                               
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* SET FILE TO XSPDIR/XSPFIL                                                     
***********************************************************************         
SETXFIL  NTR1                                                                   
         MVC   LKEY,=H'32'                                                      
         MVC   LSTATUS,=H'4'                                                    
         MVC   DATADISP,=H'42'                                                  
         MVC   SIZEIO,=F'3975'                                                  
         MVC   SYSFIL,=C'XSPFIL'                                                
         MVC   SYSDIR,=C'XSPDIR'                                                
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* SET FILE TO SPTDIR/SPTFIL                                                     
***********************************************************************         
SETSFIL  NTR1                                                                   
         MVC   LKEY,=H'13'                                                      
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'24'                                                  
         MVC   SIZEIO,=A(LIOS)                                                  
         MVC   SYSFIL,=C'SPTFIL'                                                
         MVC   SYSDIR,=C'SPTDIR'                                                
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE KEY                                                                  
***********************************************************************         
VK       DS    0H                                                               
         XC    SVMED,SVMED                                                      
         XC    SVCLT,SVCLT                                                      
         XC    SVPRD,SVPRD                                                      
         XC    SVFIC,SVFIC                                                      
         XC    SAVEKEY,SAVEKEY                                                  
*                                                                               
         CLI   ACTNUM,ACTLIST      LIST RECORDS                                 
         BE    VKL                                                              
         CLI   ACTNUM,ACTREP                                                    
         BE    VKL                                                              
*                                                                               
         BAS   RE,SETSFIL                                                       
         LA    R2,DUMMYH           VALIDATE MEDIA                               
         MVC   DUMMYH(9),=X'0900000000010000D5'                                 
         GOTO1 VALIMED                                                          
*                                                                               
         MVC   SVMED,BAGYMD                                                     
*                                                                               
         LA    R2,WBMCLTH          VALIDATE CLIENT                              
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
*                                                                               
         GOTO1 VALICLT                                                          
         MVC   SVCLT,BCLT                                                       
         MVC   WBMCLN,CLTNM                                                     
         OI    WBMCLNH+6,X'80'                                                  
*                                                                               
         LA    R2,WBMPRDH          VALIDATE PRODUCT                             
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
*                                                                               
         CLC   =C'POL',8(R2)                                                    
         BE    INVLFLD                                                          
         CLC   =C'ALL',8(R2)                                                    
         BE    INVLFLD                                                          
         CLC   =C'UNA',8(R2)                                                    
         BE    INVLFLD                                                          
         CLC   =C'AAA',8(R2)                                                    
         BE    INVLFLD                                                          
*                                                                               
         GOTO1 VALIPRD                                                          
         MVC   SVPRD,QPRD                                                       
         MVC   WBMPRN,PRDNM                                                     
         OI    WBMPRNH+6,X'80'                                                  
*                                                                               
         LA    R2,WBMFICH          VALIDATE FLIGHT INTERNAL CODE                
         BAS   RE,VALFIC                                                        
*                                                                               
         LA    R6,KEY                                                           
         USING WFLIGHTD,R6                                                      
         XC    KEY,KEY                                                          
*                                                                               
         MVI   WFKTYP,WFKTYPQ                                                   
         MVI   WFKSTYP,WFKSTYPQ                                                 
         MVC   WFKAM,SVMED         AGY/MED                                      
         MVC   WFKCLT,SVCLT        CLIENT                                       
         MVC   WFKIFLT,SVFIC       INTERNAL FLIGHT CODE                         
         BAS   RE,SETXFIL                                                       
*                                                                               
         LA    R6,KEY                                                           
         USING WFLIGHTD,R6                                                      
         XC    KEY,KEY                                                          
*                                                                               
         MVI   WFKTYP,WFKTYPQ                                                   
         MVI   WFKSTYP,WFKSTYPQ                                                 
         MVC   WFKAM,SVMED         AGY/MED                                      
         MVC   WFKCLT,SVCLT        CLIENT                                       
         MVC   WFKPRD,SVPRD        PRODUCT                                      
         MVC   WFKIFLT,SVFIC       INTERNAL FLIGHT CODE                         
*                                                                               
         BAS   RE,SETXFIL                                                       
         CLI   ACTNUM,ACTADD                                                    
         BE    VKX                                                              
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'WFKEY),KEYSAVE                                             
         BNE   RECNFND                                                          
*                                                                               
         XC    SAVEKEY,SAVEKEY                                                  
         MVC   SAVEKEY(L'WFKEY),KEY                                             
*                                                                               
VKX      B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE KEY (LIST)                                                           
***********************************************************************         
VKL      BAS   RE,SETSFIL                                                       
         XC    SVMED,SVMED                                                      
         XC    SVCLT,SVCLT                                                      
         XC    SVPRD,SVPRD                                                      
         XC    SVFIC,SVFIC                                                      
*                                                                               
         XC    KEY,KEY                                                          
         XC    SAVEKEY,SAVEKEY                                                  
*                                                                               
         LA    R2,DUMMYH           VALIDATE MEDIA                               
         MVC   DUMMYH(9),=X'0900000000010000D5'                                 
         GOTO1 VALIMED                                                          
*                                                                               
         MVC   SVMED,BAGYMD                                                     
*                                                                               
         LA    R2,WBLCLTH          VALIDATE CLIENT                              
         CLI   5(R2),0                                                          
         BE    VKL30                                                            
*                                                                               
         GOTO1 VALICLT                                                          
         MVC   SVCLT,BCLT                                                       
*                                                                               
VKL30    LA    R2,WBLPRDH          VALIDATE PRODUCT                             
         CLI   5(R2),0                                                          
         BE    VKL40                                                            
*                                                                               
         CLC   =C'POL',8(R2)                                                    
         BE    INVLFLD                                                          
         CLC   =C'ALL',8(R2)                                                    
         BE    INVLFLD                                                          
         CLC   =C'UNA',8(R2)                                                    
         BE    INVLFLD                                                          
         CLC   =C'AAA',8(R2)                                                    
         BE    INVLFLD                                                          
*                                                                               
         GOTO1 VALIPRD                                                          
         MVC   SVPRD,QPRD                                                       
*                                                                               
VKL40    LA    R2,WBLFICH          VALIDATE FLIGHT INTERNAL CODE                
         CLI   5(R2),0                                                          
         BE    VKL50                                                            
         CLI   8(R2),C'*'          ONLY LIST BLANK FLIGHT ID'S?                 
         BE    VKL50                                                            
         TM    4(R2),X'08'         NUMERIC?                                     
         BZ    INVLFLD                                                          
*                                                                               
         MVC   SVFIC,8(R2)                                                      
         OC    SVFIC,SPACES                                                     
*                                                                               
VKL50    LA    R6,KEY                                                           
         USING WFLIGHTD,R6                                                      
         XC    KEY,KEY                                                          
*                                                                               
         MVI   WFKTYP,WFKTYPQ                                                   
         MVI   WFKSTYP,WFKSTYPQ                                                 
         MVC   WFKAM,SVMED         AGY/MED                                      
         MVC   WFKCLT,SVCLT        CLIENT                                       
         MVC   WFKPRD,SVPRD        PRODUCT                                      
         MVC   WFKIFLT,SVFIC       INTERNAL FLIGHT CODE                         
*                                                                               
         BAS   RE,SETXFIL                                                       
         GOTO1 HIGH                                                             
*                                                                               
         XC    SAVEKEY,SAVEKEY                                                  
         MVC   SAVEKEY(L'WFKEY),KEY                                             
*                                                                               
VKLX     DS    0H                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY KEY                                                                   
***********************************************************************         
DK       BAS   RE,SETXFIL                                                       
         L     R6,AIO                                                           
         USING WFLIGHTD,R6                                                      
*                                                                               
         GOTO1 CLUNPK,DMCB,(BCLIAAN,WFICLT),WBMCLT                              
         MVC   WBMPRD,WFIPRD                                                    
         MVC   WBMFIC,WFIIFLT                                                   
*                                                                               
         OI    WBMCLTH+6,X'80'                                                  
         OI    WBMPRDH+6,X'80'                                                  
         OI    WBMFICH+6,X'80'                                                  
         OI    WBMCLNH+6,X'80'                                                  
         OI    WBMPRNH+6,X'80'                                                  
*                                                                               
DKX      B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* DELETE RECORD                                                                 
***********************************************************************         
RDEL     DS    0H                                                               
RDELX    DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE RECORD                                                               
***********************************************************************         
VR       CLI   ACTNUM,ACTADD                                                    
         BE    VR02                                                             
         CLC   =C'INACTIVE',WBMSTA   FLIGHT MUST BE ACTIVE TO CHANGE            
         BNE   VR02                                                             
         L     R6,AIO                                                           
         USING WFLIGHTD,R6                                                      
         TM    WFRCNTRL,WFKINACT     DID STATUS CHANGE?                         
         BO    *+12                  (CHECK THIS B/C OF SCRIPT)                 
         TM    WBMSTAH+4,X'80'                                                  
         BO    VR02                                                             
         TM    WBMFIDH+4,X'80'       INPUT AT THIS TIME?                        
         BO    INACTER               CAN'T CHANGE IF INACTIVE                   
         TM    WBMTYPH+4,X'80'                                                  
         BO    INACTER                                                          
         TM    WBMDPTH+4,X'80'                                                  
         BO    INACTER                                                          
         TM    WBMSTDH+4,X'80'                                                  
         BO    INACTER                                                          
         TM    WBMENDH+4,X'80'                                                  
         BO    INACTER                                                          
*                                                                               
VR02     XC    SVFID,SVFID                                                      
         XC    SVFTYP,SVFTYP                                                    
         XC    SVDPT,SVDPT                                                      
         XC    SVSDATE,SVSDATE                                                  
         XC    SVEDATE,SVEDATE                                                  
         XC    SVSTATUS,SVSTATUS                                                
*                                                                               
         XC    ORIGFID,ORIGFID                                                  
         XC    ORIGFIC,ORIGFIC                                                  
         XC    ORIGFTYP,ORIGFTYP                                                
         XC    ORIGDPT,ORIGDPT                                                  
         XC    ORIGSTD,ORIGSTD                                                  
         XC    ORIGEND,ORIGEND                                                  
         XC    ORIGSTAT,ORIGSTAT                                                
*                                                                               
         BAS   RE,SETXFIL                                                       
         CLI   ACTNUM,ACTADD                                                    
         BE    VR10                                                             
         CLI   ACTNUM,ACTSEL                                                    
         BNE   VR05                                                             
*                                                                               
         L     R6,AIO               SAVE AWAY KEY VALUES COMING FROM            
         USING WFLIGHTD,R6          LIST SCREEN                                 
         OC    SVCLT,SVCLT                                                      
         BNZ   *+10                                                             
         MVC   SVCLT,WFKCLT                                                     
         OC    SVPRD,SVPRD                                                      
         BNZ   *+10                                                             
         MVC   SVPRD,WFIPRD                                                     
         OC    SVFIC,SVFIC                                                      
         BNZ   *+10                                                             
         MVC   SVFIC,WFKIFLT                                                    
         DROP  R6                                                               
*                                                                               
VR05     GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
         MVC   SVDSKADD,DMDSKADD   SAVE AWAY DISK ADDRESS FOR PASSIVES          
*                                                                               
         L     R6,AIO                                                           
         USING WFLIGHTD,R6         SAVE AWAY ORIGINAL VALUES TO CHECK           
*                                  CHANGE FOR PASSIVE KEYS                      
         MVC   ORIGCLT,WFICLT      CLIENT                                       
         MVC   ORIGFID,WFIWFID     WB FLIGHT ID                                 
         MVC   ORIGFIC,WFIIFLT     INTERNAL FLIGHT ID                           
         MVC   ORIGPRD,WFIPRD      PRODUCT                                      
         MVC   ORIGDPT,WFIDPT      DAYPART                                      
         MVC   ORIGSTD,WFISTD      START DATE                                   
         MVC   ORIGEND,WFIEND      END DATE                                     
         MVC   ORIGFTYP,WFIFTYPE   DAYPART                                      
         MVC   ORIGSTAT,WFRCNTRL   STATUS                                       
*                                                                               
VR10     LA    R2,WBMFIDH          FLIGHT ID                                    
         CLI   ACTNUM,ACTADD                                                    
         BE    VR20                                                             
*                                                                               
         TM    4(R2),X'80'         INPUT AT THIS TIME?                          
         BZ    VR20                                                             
         OC    ORIGFID,ORIGFID     WAS IT BLANK BEFORE?                         
         BZ    VR20                                                             
         CLC   ORIGFID,8(R2)       YES - THEN IT CAN'T CHANGE                   
         BNE   CANTCHG                                                          
*                                                                               
VR20     CLI   5(R2),0                                                          
         BE    VR30                                                             
         CLI   5(R2),X'0A'         10 CHARACTERS                                
         BNE   INVLFLD                                                          
         TM    4(R2),X'08'         VALID NUMERIC?                               
         BZ    INVLFLD                                                          
*                                                                               
         MVC   SVFID,8(R2)                                                      
         OC    SVFID,SPACES                                                     
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    VR25                                                             
         TM    ORIGSTAT,WFKINACT   IS THIS CHANGED FROM INACTIVE?               
         BZ    VR22                                                             
         CLC   =C'ACTIVE',WBMSTA                                                
         BE    VR25                                                             
*                                                                               
VR22     TM    4(R2),X'80'         INPUT AT THIS TIME?                          
         BZ    VR30                                                             
VR25     BAS   RE,VALFID                                                        
*                                                                               
VR30     LA    R2,WBMTYPH           FLIGHT TYPE                                 
         CLI   ACTNUM,ACTADD                                                    
         BE    VR40                                                             
*                                                                               
*******  TM    4(R2),X'80'         INPUT AT THIS TIME?                          
*******  BZ    VR40                                                             
*******  CLC   ORIGFTYP,8(R2)      CAN'T CHANGE                                 
*******  BNE   CANTCHG                                                          
*                                                                               
VR40     CLI   5(R2),0              DEFAULT IS BLANK                            
         BE    VR60                                                             
         CLI   8(R2),C'C'                                                       
         BE    VR50                                                             
         CLI   8(R2),C'D'                                                       
         BE    VR50                                                             
         CLI   8(R2),C'G'                                                       
         BE    VR50                                                             
         CLI   8(R2),C'M'                                                       
         BE    VR50                                                             
         CLI   8(R2),C'T'                                                       
         BE    VR50                                                             
         CLI   8(R2),C'U'                                                       
         BE    VR50                                                             
         CLI   8(R2),C'I'                                                       
         BE    VR50                                                             
         CLI   8(R2),C'B'                                                       
         BE    VR50                                                             
         CLI   8(R2),C'J'                                                       
         BE    VR50                                                             
         CLI   8(R2),C'K'                                                       
         BE    VR50                                                             
         CLI   8(R2),C'V'                                                       
         BNE   INVLFLD                                                          
*                                                                               
VR50     MVC   SVFTYP,8(R2)                                                     
         OC    SVFTYP,SPACES                                                    
*                                                                               
VR60     LA    R2,WBMDPTH            DAYPART                                    
         TM    4(R2),X'80'           INPUT AT THIS TIME?                        
         BO    *+12                                                             
         CLI   5(R2),0               OPTIONAL                                   
         BE    VR90                                                             
*                                                                               
*******  CLI   ACTNUM,ACTADD                                                    
*******  BNE   VR70                                                             
         CLI   5(R2),0                                                          
         BE    VR90                                                             
         B     VR80                                                             
*                                                                               
VR70     CLI   WBMDPT,C' '                                                      
         BH    VR75                                                             
         CLI   ORIGDPT,C' '                                                     
         BNH   VR90                                                             
VR75     CLC   ORIGDPT,8(R2)         CAN'T CHANGE                               
         BNE   CANTCHG                                                          
*                                                                               
VR80     XC    SAVEKEY,SAVEKEY                                                  
         MVC   SAVEKEY(32),KEY                                                  
*                                                                               
         BAS   RE,SETSFIL                                                       
         GOTO1 VALIDPT                                                          
         MVC   SVDPT,QDPT                                                       
*                                                                               
         BAS   RE,SETXFIL                                                       
         XC    KEY,KEY                                                          
         MVC   KEY(32),SAVEKEY                                                  
         CLI   ACTNUM,ACTADD                                                    
         BE    VR90                                                             
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
*                                                                               
VR90     LA    R2,WBMSTDH                                                       
         GOTO1 DATVAL,DMCB,(0,WBMSTD),DUB                                       
         OC    DMCB(4),DMCB                                                     
         BZ    INVLFLD                                                          
*                                                                               
         GOTO1 DATCON,DMCB,(0,DUB),(2,SVSDATE)                                  
*                                                                               
         LA    R2,WBMENDH                                                       
         GOTO1 DATVAL,DMCB,(0,WBMEND),DUB                                       
         OC    DMCB(4),DMCB                                                     
         BZ    INVLFLD                                                          
*                                                                               
         GOTO1 DATCON,DMCB,(0,DUB),(2,SVEDATE)                                  
*                                                                               
         CLC   SVSDATE,SVEDATE      START DATE MUST BE <= END DATE              
         BH    INVDTRNG                                                         
*                                                                               
         LA    R2,WBMSTAH                                                       
         CLI   5(R2),0                                                          
         BE    VR100                                                            
*                                                                               
         CLC   =C'ACTIVE',WBMSTA                                                
         BE    VR100                                                            
         CLC   =C'INACTIVE',WBMSTA                                              
         BNE   INVLFLD                                                          
         OI    SVSTATUS,X'40'                                                   
*                                                                               
VR100    BAS   RE,VALFTDPT          VALIDATE THIS FLIGHT TYPE/DAYPART           
         BAS   RE,VALRANGE          VALIDATE THIS FLIGHT RANGE                  
*                                                                               
         L     R6,AIO                                                           
         USING WFLIGHTD,R6                                                      
*                                                                               
         MVI   WFKTYP,WFKTYPQ                                                   
         MVI   WFKSTYP,WFKSTYPQ                                                 
         MVC   WFKAM,SVMED                                                      
         MVC   WFKCLT,SVCLT                                                     
         MVC   WFKIFLT,SVFIC                                                    
*                                                                               
         MVC   WFRLEN,=X'002A'                                                  
         MVC   WFRCNTRL(1),SVSTATUS                                             
*                                                                               
         MVI   WFICODE,WFICODEQ                                                 
         MVI   WFILEN,WFILENQ                                                   
         MVC   WFIWFID,SVFID       WB FLIGHT ID                                 
         MVC   WFIIFLT,SVFIC       INTERNAL FLIGHT ID                           
         MVC   WFICLT,SVCLT        CLIENT                                       
         MVC   WFIPRD,SVPRD        PRODUCT                                      
         MVC   WFIDPT,SVDPT        DAYPART                                      
         MVC   WFISTD,SVSDATE      START DATE                                   
         MVC   WFIEND,SVEDATE      END DATE                                     
         MVC   WFIFTYPE,SVFTYP     FLIGHT TYPE                                  
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,WFRLEN                                                      
         AHI   RF,WFILENQ                                                       
         STCM  RF,3,WFRLEN                                                      
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BNE   VR110                                                            
         GOTO1 ADDREC                                                           
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(32),0(R6)                                                    
         GOTO1 HIGH                GET THE DISK ADDRESS                         
         MVC   SVDSKADD,DMDSKADD   SAVE AWAY DISK ADDRESS FOR PASSIVES          
         B     VR120                                                            
*                                                                               
VR110    GOTO1 PUTREC                                                           
*                                                                               
         LA    R6,KEY              UPDATE KEY WITH STATUS                       
         MVC   WFKCNTRL(1),SVSTATUS                                             
         GOTO1 WRITE                                                            
*                                                                               
VR120    BAS   RE,PASSIVE          PROCESS PASSIVE KEYS                         
*                                                                               
VRX      B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE FLIGHT DATE RANGES                                                   
***********************************************************************         
VALRANGE NTR1                                                                   
         CLI   ACTNUM,ACTADD                                                    
         BE    VRNGE05                                                          
         TM    WBMSTAH+4,X'80'      DID USER CHANGE STATUS?                     
         BZ    VRNGE04                                                          
         CLC   =C'ACTIVE',WBMSTA                                                
         BNE   VRNGE04                                                          
         TM    ORIGSTAT,X'40'      STATUS                                       
         BNZ   VRNGE05                                                          
*                                                                               
VRNGE04  CLC   SVSDATE,ORIGSTD      IF ACTION CHANGE, AND THE DATES             
         BNE   VRNGE05              REMAINED THE SAME, THEN NO NEED             
         CLC   SVEDATE,ORIGEND      TO VALIDATE THIS                            
         BE    VRNGX                                                            
*                                                                               
VRNGE05  LA    R6,KEY                                                           
         USING WFLIGHTD,R6                                                      
         XC    KEY,KEY                                                          
*                                                                               
         MVI   WP1TYP,WP1TYPQ       '0E8C' PASSIVE                              
         MVI   WP1STYP,WP1STYPQ     BUILD UP TO SAME MED/CLI/PRD                
         MVC   WP1AM,SVMED                                                      
         MVC   WP1CLT,SVCLT                                                     
         MVC   WP1PRD,SVPRD                                                     
         MVC   WP1FTYPE,SVFTYP                                                  
         MVC   WP1DPT,SVDPT                                                     
*                                                                               
         GOTO1 HIGH                                                             
         B     VRNG10                                                           
*                                                                               
VRNGSEQ  GOTO1 SEQ                                                              
VRNG10   CLC   KEY(WP1END-WP1KEY),KEYSAVE                                       
         BNE   VRNG100                                                          
*                                                                               
         TM    WFKCNTRL,WFKINACT     INACTIVE?                                  
         BO    VRNGSEQ               NO - SKIP                                  
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    VRNG12                                                           
         CLC   SVFID,WP1WFID                                                    
         BE    VRNGSEQ                                                          
         B     VRNG14                                                           
*                                                                               
VRNG12   CLC   SVFID,WP1WFID         CAN'T HAVE DUPLICATE FLIGHT ID'S           
         BE    DUPFLT2                                                          
*                                                                               
VRNG14   CLC   SVSDATE,WP1STD        START DATE = PASSIVE START?                
         BE    INVLRNG                                                          
         CLC   SVSDATE,WP1END        START DATE = PASSIVE END?                  
         BE    INVLRNG                                                          
         CLC   SVEDATE,WP1STD        END DATE = PASSIVE START?                  
         BE    INVLRNG                                                          
         CLC   SVEDATE,WP1END        END DATE = PASSIVE END?                    
         BE    INVLRNG                                                          
*                                                                               
         CLC   SVSDATE,WP1STD                                                   
         BH    VRNG20                                                           
         CLC   SVEDATE,WP1STD                                                   
         BH    INVLRNG                                                          
         B     VRNGSEQ                                                          
*                                                                               
VRNG20   CLC   SVSDATE,WP1END                                                   
         BH    VRNGSEQ                                                          
         B     INVLRNG                                                          
*                                                                               
VRNG100  XC    KEY,KEY                                                          
         CLI   ACTNUM,ACTADD                                                    
         BE    VRNGX                                                            
         MVC   KEY(L'WFKEY),SAVEKEY                                             
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
*                                                                               
VRNGX    DS    0H                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE FLIGHT TYPE/DAYPART                                                  
***********************************************************************         
VALFTDPT NTR1                                                                   
         CLI   ACTNUM,ACTADD                                                    
         BE    VFTDP05                                                          
*                                                                               
         TM    ORIGSTAT,X'40'       WAS IT ORIGINALLY INACTIVE?                 
         BZ    VFTDP02                                                          
         CLC   =C'ACTIVE',WBMSTA    DID USER CHANGE STATUS?                     
         BE    VFTDP05                                                          
         OC    WBMSTA,WBMSTA                                                    
         BZ    VFTDP05                                                          
         B     VFTDP04                                                          
*                                                                               
VFTDP02  TM    WBMSTAH+4,X'80'      2ND STATUS CHANGE CHECK                     
         BZ    VFTDP04                                                          
         OC    WBMSTA,WBMSTA                                                    
         BZ    *+14                                                             
         CLC   =C'ACTIVE',WBMSTA                                                
         BNE   VFTDP04                                                          
         TM    ORIGSTAT,X'40'       STATUS                                      
         BNZ   VFTDP05                                                          
*                                                                               
VFTDP04  CLC   SVFTYP,ORIGFTYP      IF ACTION CHANGE, AND THE FTYPE             
         BNE   VFTDP05              AND DPT REMAINED THE SAME, THEN             
         CLC   SVDPT,ORIGDPT        NO NEED TO VALIDATE THIS                    
         BE    VFTDPX                                                           
*                                                                               
VFTDP05  LA    R6,KEY                                                           
         USING WFLIGHTD,R6                                                      
         XC    KEY,KEY                                                          
*                                                                               
         MVI   WP1TYP,WP1TYPQ       '0E8C' PASSIVE                              
         MVI   WP1STYP,WP1STYPQ                                                 
         MVC   WP1AM,SVMED                                                      
         MVC   WP1CLT,SVCLT                                                     
         MVC   WP1PRD,SVPRD                                                     
         MVC   WP1FTYPE,SVFTYP                                                  
         MVC   WP1DPT,SVDPT                                                     
*                                                                               
         GOTO1 HIGH                                                             
         B     VFTDP10                                                          
*                                                                               
VFTDPSEQ GOTO1 SEQ                                                              
*                                                                               
VFTDP10  CLC   KEY(WP1END-WP1KEY),KEYSAVE                                       
         BNE   VFTDP100                                                         
*                                                                               
         TM    WFKCNTRL,WFKINACT     INACTIVE?                                  
         BO    VFTDPSEQ              NO NEED TO VALIDATE AGAINST THIS           
*                                                                               
         CLC   SVSDATE,WP1END        START > THIS END?                          
         BH    VFTDP100                                                         
         CLC   SVEDATE,WP1STD        END < THIS START?                          
         BL    VFTDP100                                                         
*                                                                               
         B     INVFTDPT                                                         
*                                                                               
VFTDP100 DS    0H                                                               
         XC    KEY,KEY                                                          
         CLI   ACTNUM,ACTADD                                                    
         BE    VFTDPX                                                           
         MVC   KEY(L'WFKEY),SAVEKEY                                             
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
*                                                                               
VFTDPX   DS    0H                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE INTERNAL FLIGHT ID                                                   
***********************************************************************         
VALFIC   NTR1                                                                   
         LA    R2,WBMFICH          VALIDATE FLIGHT INTERNAL CODE                
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         TM    4(R2),X'08'         MUST BE NUMERIC                              
         BZ    INVLFLD                                                          
         CLI   5(R2),6                                                          
         BNE   INVLFLD                                                          
*                                                                               
         MVC   SVFIC,8(R2)                                                      
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BNE   VALFICX                                                          
*                                                                               
         BAS   RE,SETXFIL                                                       
         XC    KEY,KEY                                                          
*                                                                               
         LA    R6,KEY                                                           
         USING WFLIGHTD,R6                                                      
*                                                                               
         MVI   WFKTYP,WFKTYPQ                                                   
         MVI   WFKSTYP,WFKSTYPQ                                                 
         MVC   WFKAM,SVMED         AGY/MED                                      
         MVC   WFKCLT,SVCLT        CLIENT                                       
         GOTO1 HIGH                                                             
         B     VFIC20                                                           
*                                                                               
VFICSEQ  GOTO1 SEQ                                                              
VFIC20   CLC   KEY(WFKPRD-WFKEY),KEYSAVE    SAME CLIENT?                        
         BNE   VALFICX                                                          
*                                                                               
         CLC   SVFIC,WFKIFLT       SAME INTERNAL FLIGHT CODE?                   
         BE    DUPFLT1                                                          
         B     VFICSEQ                                                          
*                                                                               
VALFICX  DS    0H                                                               
         B     EXIT                                                             
         DROP  R6                                                               
***********************************************************************         
* VALIDATE WB FLIGHT ID                                                         
***********************************************************************         
VALFID   NTR1                                                                   
         BAS   RE,SETXFIL                                                       
         MVC   SAVEKEY,KEY                                                      
         XC    KEY,KEY                                                          
*                                                                               
         LA    R6,KEY                                                           
         USING WFLIGHTD,R6                                                      
*                                                                               
         MVI   WP2TYP,WP2TYPQ                                                   
         MVI   WP2STYP,WP2STYPQ                                                 
         MVC   WP2AM,SVMED         AGY/MED                                      
         MVC   WP2WFID,SVFID       WB FLIGHT ID                                 
         GOTO1 HIGH                                                             
         B     VFID20                                                           
*                                                                               
VFIDSEQ  GOTO1 SEQ                                                              
VFID20   CLC   KEY(WP2IFLT-WP2KEY),KEYSAVE    SAME WB FLIGHT ID?                
         BNE   VALFIDX                                                          
*&&DO                                                                           
         TM    WFKCNTRL,WFKINACT     INACTIVE?                                  
         BO    VFIDSEQ               NO - SKIP                                  
*&&                                                                             
         CLC   SVCLT,WP2CLT        EXISTS FOR ANOTHER CLIENT?                   
         BNE   VFIDSEQ                                                          
         CLI   ACTNUM,ACTADD                                                    
         BE    DUPFLT2                                                          
         CLC   SVFIC,WP2IFLT       IS IT THE SAME RECORD?                       
         BNE   DUPFLT2                                                          
         CLC   SVPRD,WP2PRD                                                     
         BNE   DUPFLT2                                                          
         B     VFIDSEQ                                                          
*                                                                               
VALFIDX  DS    0H                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    EXIT                                                             
         XC    KEY,KEY             IF IT'S A CHANGE, RESTORE KEY                
         MVC   KEY(32),SAVEKEY                                                  
         GOTO1 HIGH                                                             
         B     EXIT                                                             
         DROP  R6                                                               
***********************************************************************         
* PROCESS PASSIVE KEYS                                                          
***********************************************************************         
PASSIVE  NTR1                                                                   
         BAS   RE,SETXFIL                                                       
         MVC   SAVEKEY(32),KEY                                                  
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    PSSV50                                                           
*                                                                               
         XC    KEY,KEY              DELETE ORIG PSSVKEY SINCE                   
         LA    R6,KEY               KEY FIELDS CHANGED                          
         USING WFLIGHTD,R6                                                      
*                                                                               
         MVI   WP1TYP,WP1TYPQ       '0E8C' PASSIVE                              
         MVI   WP1STYP,WP1STYPQ                                                 
         MVC   WP1AM,SVMED                                                      
         MVC   WP1CLT,SVCLT                                                     
         MVC   WP1PRD,SVPRD                                                     
         MVC   WP1DPT,ORIGDPT                                                   
         MVC   WP1STD,ORIGSTD                                                   
         MVC   WP1END,ORIGEND                                                   
         MVC   WP1FTYPE,ORIGFTYP                                                
         MVC   WP1WFID,ORIGFID                                                  
         MVC   WP1IFLT,ORIGFIC                                                  
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'WFKEY),KEYSAVE                                             
         BE    *+6                                                              
         DC    H'00'                                                            
         OI    WFKCNTRL,X'80'                                                   
         GOTO1 WRITE                                                            
*                                                                               
         XC    KEY,KEY              DELETE ORIG PSSVKEY SINCE                   
         LA    R6,KEY               KEY FIELDS CHANGED                          
         USING WFLIGHTD,R6                                                      
*                                                                               
         MVI   WP2TYP,WP2TYPQ       '0E9C' PASSIVE                              
         MVI   WP2STYP,WP2STYPQ                                                 
         MVC   WP2AM,SVMED                                                      
         MVC   WP2WFID,ORIGFID                                                  
         MVC   WP2IFLT,ORIGFIC                                                  
         MVC   WP2CLT,SVCLT                                                     
         MVC   WP2PRD,SVPRD                                                     
         MVC   WP2STD,ORIGSTD                                                   
         MVC   WP2END,ORIGEND                                                   
         MVC   WP2FTYPE,ORIGFTYP                                                
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'WFKEY),KEYSAVE                                             
         BE    *+6                                                              
         DC    H'00'                                                            
         OI    WFKCNTRL,X'80'                                                   
         GOTO1 WRITE                                                            
*                                                                               
PSSV50   XC    KEY,KEY              ADD NEW PASSIVE KEYS                        
         LA    R6,KEY                                                           
*                                                                               
         MVI   WP1TYP,WP1TYPQ       '0E8C' PASSIVE                              
         MVI   WP1STYP,WP1STYPQ                                                 
         MVC   WP1AM,SVMED                                                      
         MVC   WP1CLT,SVCLT                                                     
         MVC   WP1PRD,SVPRD                                                     
         MVC   WP1DPT,SVDPT                                                     
         MVC   WP1STD,SVSDATE                                                   
         MVC   WP1END,SVEDATE                                                   
         MVC   WP1FTYPE,SVFTYP                                                  
         MVC   WP1WFID,SVFID                                                    
         MVC   WP1IFLT,SVFIC                                                    
*                                                                               
         OI    DMINBTS,X'08'        CHECK IF DELETED KEY EXISTS                 
         GOTO1 HIGH                                                             
         CLC   KEY(L'WFKEY),KEYSAVE                                             
         BNE   PSSV60                                                           
         MVC   WFKCNTRL(1),SVSTATUS                                             
         MVC   WFKDA,SVDSKADD                                                   
         GOTO1 WRITE                                                            
         B     PSSV70                                                           
*                                                                               
PSSV60   DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'WFKEY),KEYSAVE                                             
         MVC   WFKCNTRL(1),SVSTATUS                                             
         MVC   WFKDA,SVDSKADD                                                   
*                                                                               
         GOTO1 ADD                                                              
*                                                                               
PSSV70   XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
*                                                                               
         MVI   WP2TYP,WP2TYPQ       '0E9C' PASSIVE                              
         MVI   WP2STYP,WP2STYPQ                                                 
         MVC   WP2AM,SVMED                                                      
         MVC   WP2WFID,SVFID                                                    
         MVC   WP2IFLT,SVFIC                                                    
         MVC   WP2CLT,SVCLT                                                     
         MVC   WP2PRD,SVPRD                                                     
         MVC   WP2STD,SVSDATE                                                   
         MVC   WP2END,SVEDATE                                                   
         MVC   WP2FTYPE,SVFTYP                                                  
*                                                                               
         OI    DMINBTS,X'08'        CHECK IF DELETED KEY EXISTS                 
         GOTO1 HIGH                                                             
         CLC   KEY(L'WFKEY),KEYSAVE                                             
         BNE   PSSV80                                                           
         MVC   WFKCNTRL(1),SVSTATUS                                             
         MVC   WFKDA,SVDSKADD                                                   
         GOTO1 WRITE                                                            
         B     PSSV90                                                           
*                                                                               
PSSV80   DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'WFKEY),KEYSAVE                                             
         MVC   WFKCNTRL(1),SVSTATUS                                             
         MVC   WFKDA,SVDSKADD                                                   
*                                                                               
         GOTO1 ADD                                                              
*                                                                               
PSSV90   XC    KEY,KEY                                                          
         MVC   KEY(L'WFKEY),SAVEKEY                                             
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
*                                                                               
PSSVX    DS    0H                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY RECORD                                                                
***********************************************************************         
DR       DS    0H                                                               
         BAS   RE,SETXFIL                                                       
         L     R6,AIO                                                           
         USING WFLIGHTD,R6                                                      
*                                                                               
         MVC   WBMFID(L'WFIWFID),WFIWFID                                        
         MVC   WBMTYP(L'WFIFTYPE),WFIFTYPE                                      
         OC    WBMUPD,SPACES                                                    
*                                                                               
         GOTO1 DATCON,DMCB,(2,WFISTD),(5,WBMSTD)                                
         GOTO1 DATCON,DMCB,(2,WFIEND),(5,WBMEND)                                
*                                                                               
         MVC   WBMSTA(8),=C'ACTIVE  '                                           
         TM    WFRCNTRL,WFKINACT                                                
         BZ    *+10                                                             
         MVC   WBMSTA(8),=C'INACTIVE'                                           
*                                                                               
         MVC   QDPT2,SPACES                                                     
         MVC   SAVEKEY,0(R6)                                                    
         BAS   RE,SETSFIL                                                       
         GOTO1 VALIDPT,DMCB,(X'01',WFIDPT)                                      
         MVC   WBMDPT(L'QDPT2),QDPT2                                            
*                                                                               
         BAS   RE,SETXFIL                                                       
         XC    KEY,KEY                                                          
         MVC   KEY(L'WFKEY),SAVEKEY                                             
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'F1'         GET ACTIVITY ELEMENT                        
         BAS   RE,GETEL                                                         
         BNE   DR50                                                             
*                                                                               
         MVC   WBMUPD(13),=C'LAST UPDATED:'                                     
         GOTO1 DATCON,DMCB,(3,2(R6)),(5,WBMUPD+14)                              
         CLI   8(R6),0              ANY CHANGE DATE?                            
         BE    DR20                                                             
         GOTO1 DATCON,DMCB,(3,8(R6)),(5,WBMUPD+14)                              
*                                                                               
DR20     DS    0H                                                               
         MVC   WBMUPD+24(8),20(R6)                                              
         OC    WBMUPD,SPACES                                                    
*                                                                               
DR50     OI    WBMFIDH+6,X'80'                                                  
         OI    WBMTYPH+6,X'80'                                                  
         OI    WBMDPTH+6,X'80'                                                  
         OI    WBMSTDH+6,X'80'                                                  
         OI    WBMENDH+6,X'80'                                                  
         OI    WBMSTAH+6,X'80'                                                  
         OI    WBMUPDH+6,X'80'                                                  
*                                                                               
DRX      DS    0H                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* LIST RECORDS *                                                                
***********************************************************************         
LR       DS    0H                                                               
         LA    R2,LISTAR                                                        
         USING LISTRECD,R2                                                      
         BAS   RE,SETXFIL                                                       
*                                                                               
         OC    KEY,KEY             FIRST TIME THROUGH                           
         BNZ   *+10                                                             
         MVC   KEY(32),SAVEKEY                                                  
         GOTO1 HIGH                                                             
         B     LR10                                                             
*                                                                               
LRSEQ    GOTO1 SEQ                                                              
*                                                                               
LR10     DS    0H                                                               
         LA    R6,KEY                                                           
         USING WFLIGHTD,R6                                                      
*                                                                               
         CLI   WFKTYP,WFKTYPQ                                                   
         BNE   LRX                                                              
         CLI   WFKSTYP,WFKSTYPQ                                                 
         BNE   LRX                                                              
         CLC   WFKAM,BAGYMD                                                     
         BNE   LRSEQ                                                            
*                                                                               
         OC    SVCLT,SVCLT                                                      
         BZ    *+14                                                             
         CLC   WFKCLT,SVCLT         ANY CLIENT FILTER?                          
         BNE   LRSEQ                                                            
*                                                                               
         OC    SVPRD,SVPRD                                                      
         BZ    *+14                                                             
         CLC   WFKPRD,SVPRD         ANY PRODUCT FILTER?                         
         BNE   LRSEQ                                                            
*                                                                               
         OC    SVFIC,SVFIC                                                      
         BZ    *+14                                                             
         CLC   WFKIFLT,SVFIC        ANY INTERNAL FLIGHT ID  FILTER?             
         BNE   LRSEQ                                                            
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         CLI   WBLFIC,C'*'                                                      
         BNE   *+14                                                             
         OC    WFIWFID,WFIWFID                                                  
         BNZ   LRSEQ                                                            
*                                                                               
         L     R6,AIO                                                           
         GOTO1 CLUNPK,DMCB,(BCLIAAN,WFICLT),LCLT                                
         MVC   LPRD,WFIPRD                                                      
         MVC   LFIC,WFIIFLT                                                     
         MVC   LFID,WFIWFID                                                     
         GOTO1 DATCON,DMCB,(2,WFISTD),(5,LSDATE)                                
         MVI   LSDATE+L'LSDATE,C'-'                                             
         GOTO1 DATCON,DMCB,(2,WFIEND),(5,LEDATE)                                
         MVC   LFTYP,WFIFTYPE                                                   
*                                                                               
         MVI   LSTAT,C'A'                                                       
         TM    WFRCNTRL,WFKINACT                                                
         BZ    *+8                                                              
         MVI   LSTAT,C'I'                                                       
*                                                                               
         MVC   QDPT2,SPACES                                                     
         MVC   SAVEKEY,KEY                                                      
         BAS   RE,SETSFIL                                                       
         GOTO1 VALIDPT,DMCB,(X'01',WFIDPT)                                      
         MVC   LDPT,QDPT2                                                       
*                                                                               
         BAS   RE,SETXFIL                                                       
         XC    KEY,KEY                                                          
         MVC   KEY(L'WFKEY),SAVEKEY                                             
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
*                                                                               
         GOTO1 LISTMON                                                          
         MVC   LISTAR,SPACES                                                    
         B     LRSEQ                                                            
*                                                                               
LRX      DS    0H                                                               
         XC    KEY,KEY                                                          
         XC    SAVEKEY,SAVEKEY                                                  
         B     EXIT                                                             
         DROP  R6,R2                                                            
         EJECT                                                                  
***********************************************************************         
* PRINT RECORDS                                                                 
***********************************************************************         
PR       DS    0H                                                               
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         LA    R1,HDRTN                                                         
         ST    R1,HEADHOOK                                                      
*                                                                               
         CLC   =C'DOWN',CONOUT                                                  
         BNE   PR05                                                             
         XC    SPECS,SPECS                                                      
         XC    HEADHOOK,HEADHOOK                                                
         MVC   P(L'DOWNHEAD),DOWNHEAD                                           
         MVI   P+1+L'DOWNHEAD,X'5E'                                             
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PR05     LA    R2,P                                                             
         USING PRNTRECD,R2                                                      
         BAS   RE,SETXFIL                                                       
*                                                                               
         OC    KEY,KEY             FIRST TIME THROUGH                           
         BNZ   *+10                                                             
         MVC   KEY(32),SAVEKEY                                                  
         GOTO1 HIGH                                                             
         B     PR10                                                             
*                                                                               
PRSEQ    GOTO1 SEQ                                                              
*                                                                               
PR10     DS    0H                                                               
         LA    R6,KEY                                                           
         USING WFLIGHTD,R6                                                      
*                                                                               
         CLI   WFKTYP,WFKTYPQ                                                   
         BNE   PRX                                                              
         CLI   WFKSTYP,WFKSTYPQ                                                 
         BNE   PRX                                                              
*                                                                               
         OC    SVCLT,SVCLT                                                      
         BZ    *+14                                                             
         CLC   WFKCLT,SVCLT         ANY CLIENT FILTER?                          
         BNE   PRSEQ                                                            
*                                                                               
         OC    SVPRD,SVPRD                                                      
         BZ    *+14                                                             
         CLC   WFKPRD,SVPRD         ANY PRODUCT FILTER?                         
         BNE   PRSEQ                                                            
*                                                                               
         OC    SVFIC,SVFIC                                                      
         BZ    *+14                                                             
         CLC   WFKIFLT,SVFIC        ANY INTERNAL FLIGHT ID  FILTER?             
         BNE   PRSEQ                                                            
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         CLC   =C'DOWN',CONOUT                                                  
         BNE   PR50                                                             
*                                                                               
         L     R6,AIO                                                           
         LA    R2,P                                                             
*                                                                               
         MVI   0(R2),C'"'                                                       
         AHI   R2,1                                                             
         GOTO1 CLUNPK,DMCB,(BCLIAAN,WFICLT),0(R2)                               
         AHI   R2,L'PCLT                                                        
         MVI   0(R2),C'"'                                                       
         AHI   R2,1                                                             
*                                                                               
         MVI   0(R2),C'"'                                                       
         AHI   R2,1                                                             
         MVC   0(L'WFIPRD,R2),WFIPRD                                            
         AHI   R2,L'PPRD                                                        
         MVI   0(R2),C'"'                                                       
         AHI   R2,1                                                             
*                                                                               
         MVI   0(R2),C'"'                                                       
         AHI   R2,1                                                             
         MVC   0(L'WFIIFLT,R2),WFIIFLT                                          
         AHI   R2,L'PFIC                                                        
         MVI   0(R2),C'"'                                                       
         AHI   R2,1                                                             
*                                                                               
         MVI   0(R2),C'"'                                                       
         AHI   R2,1                                                             
         MVC   0(L'WFIWFID,R2),WFIWFID                                          
         AHI   R2,L'PFID                                                        
         MVI   0(R2),C'"'                                                       
         AHI   R2,1                                                             
*                                                                               
         MVI   0(R2),C'"'                                                       
         AHI   R2,1                                                             
         GOTO1 DATCON,DMCB,(2,WFISTD),(5,0(R2))                                 
         AHI   R2,8                                                             
         MVI   0(R2),C'-'                                                       
         AHI   R2,1                                                             
         GOTO1 DATCON,DMCB,(2,WFIEND),(5,0(R2))                                 
         AHI   R2,8                                                             
         MVI   0(R2),C'"'                                                       
         AHI   R2,1                                                             
*                                                                               
         MVI   0(R2),C'"'                                                       
         AHI   R2,1                                                             
         MVC   0(L'WFIFTYPE,R2),WFIFTYPE                                        
         AHI   R2,L'PFTYP                                                       
         MVI   0(R2),C'"'                                                       
         AHI   R2,1                                                             
*                                                                               
         MVI   0(R2),C'"'                                                       
         AHI   R2,1                                                             
         MVI   0(R2),C'A'                                                       
         TM    WFRCNTRL,WFKINACT                                                
         BZ    *+8                                                              
         MVI   0(R2),C'I'                                                       
         AHI   R2,1                                                             
         MVI   0(R2),C'"'                                                       
         AHI   R2,1                                                             
*                                                                               
         MVC   SAVEKEY,KEY                                                      
         MVC   QDPT2,SPACES                                                     
         BAS   RE,SETSFIL                                                       
         MVI   0(R2),C'"'                                                       
         AHI   R2,1                                                             
         GOTO1 VALIDPT,DMCB,(X'01',WFIDPT)                                      
         MVC   0(L'QDPT2,R2),QDPT2                                              
         AHI   R2,L'PDPT                                                        
         MVI   0(R2),C'"'                                                       
         AHI   R2,1                                                             
*                                                                               
         MVI   0(R2),X'5E'                                                      
         MVI   LINE,0                                                           
*                                                                               
         BAS   RE,SETXFIL                                                       
         XC    KEY,KEY                                                          
         MVC   KEY(L'WFKEY),SAVEKEY                                             
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     PRSEQ                                                            
*                                                                               
PR50     L     R6,AIO                                                           
         GOTO1 CLUNPK,DMCB,(BCLIAAN,WFICLT),PCLT                                
         MVC   PPRD,WFIPRD                                                      
         MVC   PFIC,WFIIFLT                                                     
         MVC   PFID,WFIWFID                                                     
         GOTO1 DATCON,DMCB,(2,WFISTD),(5,PSDATE)                                
         MVI   PSDATE+L'PSDATE,C'-'                                             
         GOTO1 DATCON,DMCB,(2,WFIEND),(5,PEDATE)                                
         MVC   PFTYP,WFIFTYPE                                                   
*                                                                               
         MVI   PSTAT,C'A'                                                       
         TM    WFRCNTRL,WFKINACT                                                
         BZ    *+8                                                              
         MVI   PSTAT,C'I'                                                       
*                                                                               
         MVC   QDPT2,SPACES                                                     
         MVC   SAVEKEY,KEY                                                      
         BAS   RE,SETSFIL                                                       
         GOTO1 VALIDPT,DMCB,(X'01',WFIDPT)                                      
         MVC   PDPT,QDPT2                                                       
*                                                                               
         BAS   RE,SETXFIL                                                       
         XC    KEY,KEY                                                          
         MVC   KEY(L'WFKEY),SAVEKEY                                             
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     PRSEQ                                                            
*                                                                               
PRX      DS    0H                                                               
         XC    KEY,KEY                                                          
         XC    SAVEKEY,SAVEKEY                                                  
         B     EXIT                                                             
         DROP  R6,R2                                                            
         EJECT                                                                  
DOWNHEAD DC    C'"CLI" "PRD" "INTERN FL" "WB FLIGHT" "S-E PERIOD" "FL TX        
               YPE" "DAYPT" "STATUS"'                                           
***********************************************************************         
* HEADER AND HEAD HOOK ROUTINES                                                 
***********************************************************************         
HEADING  DS    0H                                                               
         SSPEC H1,3,REQUESTOR                                                   
         SSPEC H1,41,C'          WB FLIGHT REPORT            '                  
         SSPEC H2,41,C'          ----------------            '                  
         SSPEC H1,93,AGYNAME                                                    
         SSPEC H2,3,C'CLIENT:'                                                  
         SSPEC H2,93,AGYADD                                                     
         SSPEC H3,3,C'PRODUCT:'                                                 
         SSPEC H3,93,REPORT                                                     
         SSPEC H4,3,C'FLIGHT INTERNAL CODE:'                                    
         SSPEC H4,93,RUN                                                        
         SSPEC H5,103,PAGE                                                      
         DC    X'00'                                                            
*                                                                               
HDRTN    NTR1                                                                   
         MVC   H2+11(3),WBLCLT                                                  
         MVC   H3+11(3),WBLPRD                                                  
         MVC   H4+24(6),WBLFIC                                                  
*                                                                               
         LA    RF,H6                                                            
         USING PRNTRECD,RF                                                      
*                                                                               
         MVC   PCLT,=C'CLI'                                                     
         MVC   PPRD,=C'PRD'                                                     
         MVC   PFIC(9),=C'INTERN FL'                                            
         MVC   PFID(11),=C' WB FLIGHT '                                         
         MVC   PSDATE(16),=C'  S-E PERIOD    '                                  
         MVC   PFTYP-3(7),=C'FL TYPE'                                           
         MVC   PDPT-2(5),=C'DAYPT'                                              
         MVC   PSTAT-2(6),=C'STATUS'                                            
*                                                                               
         LA    RF,H7                                                            
         MVC   PCLT,=C'---'                                                     
         MVC   PPRD,=C'---'                                                     
         MVC   PFIC(9),=C'---------'                                            
         MVC   PFID(11),=C'-----------'                                         
         MVC   PSDATE(16),=C'----------------'                                  
         MVC   PFTYP-3(7),=C'-------'                                           
         MVC   PDPT-2(5),=C'-----'                                              
         MVC   PSTAT-2(6),=C'------'                                            
*                                                                               
HDRTNX   B     EXIT                                                             
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* DATAMGR INTERFACE                                                             
***********************************************************************         
MYFILADD NTR1                                                                   
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'SPTFILE ',KEY+14,AIO,MYDMWRK           
         BAS   RE,DMCHECK                                                       
         B     DMYES                                                            
*                                                                               
MYFILWRT NTR1                                                                   
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'SPTFILE ',KEY+14,AIO,MYDMWRK           
         BAS   RE,DMCHECK                                                       
         B     DMYES                                                            
*                                                                               
MYDIRWRT NTR1                                                                   
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'SPTDIR  ',KEY,KEY                      
         BAS   RE,DMCHECK                                                       
         B     DMYES                                                            
*                                                                               
MYDIRADD NTR1                                                                   
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'SPTDIR  ',KEY,KEY                      
         BAS   RE,DMCHECK                                                       
         B     DMYES                                                            
*                                                                               
DMCHECK  CLI   8(R1),0                                                          
         BER   RE                                                               
         TM    8(R1),X'90'                                                      
         BM    DMNO                                                             
         DC    H'0'                                                             
         SPACE 1                                                                
DMYES    SR    R1,R1                                                            
         B     *+8                                                              
DMNO     LA    R1,1                                                             
         LTR   R1,R1                                                            
         SPACE 1                                                                
XIT      XIT1  REGS=(R0,R1)                                                     
         EJECT                                                                  
***********************************************************************         
* ERROR MESSAGES                                                                
***********************************************************************         
INVLFLD  MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
MISSFLD  MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
RECNFND  MVI   ERROR,NOTFOUND                                                   
         B     TRAPERR                                                          
*                                                                               
INVLACT  MVI   ERROR,INVACT                                                     
         LA    R2,CONACTH                                                       
         B     TRAPERR                                                          
*                                                                               
DELERR   LA    R2,CONACTH                                                       
         MVC   ERRNUM,=AL2(ACTNERR)                                             
         B     SPERREX                                                          
*                                                                               
INVLRNG  LA    R2,WBMSTDH                                                       
         MVC   ERRNUM,=AL2(OVLPERR)                                             
         B     SPERREX                                                          
*                                                                               
DUPFLT1  LA    R2,WBMFICH                                                       
         B     *+8                                                              
DUPFLT2  LA    R2,WBMFIDH                                                       
         MVC   ERRNUM,=AL2(DUPFERR)                                             
         B     SPERREX                                                          
*                                                                               
CANTCHG  MVC   ERRNUM,=AL2(CNTCERR)                                             
         B     SPERREX                                                          
*                                                                               
INACTER  LA    R2,WBMSTAH                                                       
         MVC   ERRNUM,=AL2(FACTERR)                                             
         B     SPERREX                                                          
*                                                                               
INVFTDPT LA    R2,WBMTYPH                                                       
         MVC   ERRNUM,=AL2(FTDPERR)                                             
         B     SPERREX                                                          
*                                                                               
INVDTRNG LA    R2,WBMSTDH                                                       
         MVC   ERRNUM,=AL2(DTRNERR)                                             
         B     SPERREX                                                          
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
TRAPERR2 GOTO1 ERREX2                                                           
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
*                                                                               
ACTNERR  EQU   1310                                                             
OVLPERR  EQU   1311                                                             
DUPFERR  EQU   1312                                                             
CNTCERR  EQU   1313                                                             
FACTERR  EQU   1314                                                             
FTDPERR  EQU   1319                                                             
DTRNERR  EQU   1320                                                             
*                                                                               
         GETEL R6,42,ELCODE                                                     
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
       ++INCLUDE NESFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NESFM9CD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NESFM9BD                                                       
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE NESFMWORKD                                                     
         ORG   SYSSPARE+220                                                     
*                           *******  T31C47 WORK AREA  *******                  
WORKAREA DS    0CL1                                                             
MYDMWRK  DS    12D                                                              
PREVKEY  DS    CL48                                                             
SAVEKEY  DS    CL48                                                             
*                                                                               
ERRNUM   DS    XL2                                                              
*                                                                               
SVDATE   DS    XL2                                                              
SVMED    DS    XL1                                                              
SVCLT    DS    XL2                                                              
SVFIC    DS    CL6                                                              
SVPRD    DS    CL3                                                              
SVFID    DS    CL10                                                             
SVFTYP   DS    CL1                                                              
SVDPT    DS    XL1                                                              
SVSDATE  DS    XL2                                                              
SVEDATE  DS    XL2                                                              
SVSTATUS DS    XL1                                                              
SVDSKADD DS    F                                                                
*                                                                               
ORIGCLT  DS    XL2                                                              
ORIGPRD  DS    CL3                                                              
ORIGFID  DS    CL10                                                             
ORIGFIC  DS    CL6                                                              
ORIGFTYP DS    CL1                                                              
ORIGDPT  DS    XL1                                                              
ORIGSTD  DS    XL2                                                              
ORIGEND  DS    XL2                                                              
ORIGSTAT DS    XL1                                                              
*                                                                               
PSSVKEY  DS    XL50                                                             
*                                                                               
MYFLAG   DS    XL1                                                              
CHGPSSV  EQU   X'01'                THERE'S A CHANGE SO DEL/ADD PSSV            
*                                                                               
DUMMYH   DS    XL8                                                              
DUMMY    DS    XL20                                                             
*                                                                               
WORKEND  EQU   *                                                                
         EJECT                                                                  
*                                                                               
LIOS     EQU   4000                                                             
*                                                                               
       ++INCLUDE SPGENWBFLT                                                     
       ++INCLUDE FAGETTXTD                                                      
*                                                                               
LISTRECD DSECT                                                                  
LCLT     DS    CL3                 CLIENT                                       
         DS    CL1                                                              
LPRD     DS    CL3                 PRODUCT 1                                    
         DS    CL1                                                              
LFIC     DS    CL6                 FLIGHT INTERNAL ID                           
         DS    CL4                                                              
LFID     DS    CL10                WB FLIGHT ID                                 
         DS    CL2                                                              
LSDATE   DS    CL8                 START DATE                                   
         DS    CL1                                                              
LEDATE   DS    CL8                 END DATE                                     
         DS    CL4                                                              
LFTYP    DS    CL1                 FLIGHT TYPE                                  
         DS    CL7                                                              
LDPT     DS    CL2                 DAYPART                                      
         DS    CL5                                                              
LSTAT    DS    CL1                 ACTIVE/INACTIVE                              
*                                                                               
PRNTRECD DSECT                                                                  
         DS    CL2                                                              
PCLT     DS    CL3                 CLIENT                                       
         DS    CL1                                                              
PPRD     DS    CL3                 PRODUCT 1                                    
         DS    CL1                                                              
PFIC     DS    CL6                 FLIGHT INTERNAL ID                           
         DS    CL4                                                              
PFID     DS    CL10                WB FLIGHT ID                                 
         DS    CL2                                                              
PSDATE   DS    CL8                 START DATE                                   
         DS    CL1                                                              
PEDATE   DS    CL8                 END DATE                                     
         DS    CL4                                                              
PFTYP    DS    CL1                 FLIGHT TYPE                                  
         DS    CL7                                                              
PDPT     DS    CL2                 DAYPART                                      
         DS    CL5                                                              
PSTAT    DS    CL1                 ACTIVE/INACTIVE                              
*                                                                               
         PRINT GEN                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'058NESFM57   08/23/16'                                      
         END                                                                    
