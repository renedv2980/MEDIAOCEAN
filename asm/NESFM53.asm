*          DATA SET NESFM53    AT LEVEL 187 AS OF 06/24/08                      
*PHASE T31C53A,*                                                                
***********************************************************************         
*                                                                               
*  TITLE: T31C53 - LIST OF AUTOPAY                                              
*                                                                               
***********************************************************************         
         TITLE 'T31C53 NETWORK AUTOPAY'                                         
T31C53   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,AUTOPAY,R7                                                     
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         MVI   IOOPT,C'Y'          CONTROL MY OWN ADDREC/PUTREC                 
*                                                                               
         GOTO1 VTERMACC            CHECK FOR DISP/LIST ONLY TERMINALS           
*                                                                               
         BAS   RE,SETXFIL          SET TO XSPDIR/XSPFIL                         
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD KEY                          
         BE    VR                                                               
         CLI   MODE,DISPKEY        VALIDATE RECORD KEY                          
         BE    DK                                                               
         CLI   MODE,DISPREC        VALIDATE RECORD KEY                          
         BE    DR                                                               
         CLC   =C'CHA',CONACT                                                   
         BE    INVLACT                                                          
         CLC   =C'ADD',CONACT                                                   
         BE    INVLACT                                                          
*                                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BNE   *+12                                                             
         LA    R2,LISTAR                                                        
         B     LR                                                               
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BNE   *+12                                                             
         LA    R2,P                                                             
         B     LR                                                               
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
         CLI   ACTNUM,ACTLIST      LIST RECORDS                                 
         BE    VKL                                                              
         CLI   ACTNUM,ACTREP                                                    
         BE    VKL                                                              
*                                                                               
         XC    KEY,KEY                                                          
         XC    SVDATE,SVDATE                                                    
         XC    SVMED,SVMED                                                      
         XC    SVCLT,SVCLT                                                      
         XC    SVPRD,SVPRD                                                      
         XC    SVPTN,SVPTN                                                      
         XC    SVEST,SVEST                                                      
         XC    SVSTA,SVSTA                                                      
         XC    SVMON,SVMON                                                      
*                                                                               
VK10     DS    0H                                                               
         BAS   RE,SETSFIL                                                       
         LA    R2,MNTMEDH          VALIDATE MEDIA                               
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         GOTO1 VALIMED                                                          
*                                                                               
         MVC   SVMED,BAGYMD                                                     
         MVC   LSTMDN,MEDNM                                                     
         OI    LSTMDNH+6,X'80'                                                  
*                                                                               
VK20     DS    0H                                                               
         LA    R2,MNTCLTH          VALIDATE CLIENT                              
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
*                                                                               
         GOTO1 VALICLT                                                          
         MVC   SVCLT,BCLT                                                       
         MVC   MNTCLN,CLTNM                                                     
         OI    MNTCLNH+6,X'80'                                                  
*                                                                               
VK30     DS    0H                                                               
         LA    R2,MNTPRDH          VALIDATE PRODUCT                             
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
*                                                                               
         GOTO1 VALIPRD                                                          
         MVC   SVPRD,BPRD                                                       
         MVC   MNTPDN,PRDNM                                                     
         OI    MNTPDNH+6,X'80'                                                  
*                                                                               
VK40     DS    0H                                                               
         LA    R2,MNTPTRH          VALIDATE PRODUCT                             
         CLI   5(R2),0                                                          
         BE    VK50                                                             
*                                                                               
         GOTO1 VALIPRD                                                          
         MVC   SVPTN,BPRD                                                       
         MVC   MNTPDN,PRDNM                                                     
         OI    MNTPDNH+6,X'80'                                                  
*                                                                               
VK50     DS    0H                                                               
         LA    R2,MNTESTH          VALIDATE ESTIMATE                            
         CLI   5(R2),0                                                          
         BE    VK60                                                             
*                                                                               
         XC    BEST,BEST                                                        
*                                                                               
         GOTO1 VALIEST                                                          
         MVC   SVEST,BEST                                                       
         L     R6,AIO                                                           
         USING ESTHDR,R6                                                        
         GOTO1 DATCON,DMCB,(0,ESTART),(10,MNTESD)                               
         GOTO1 DATCON,DMCB,(0,EEND),(10,MNTESD+9)                               
         MVI   MNTESD+8,C'-'                                                    
         OI    MNTESDH+6,X'80'                                                  
         DROP  R6                                                               
*                                                                               
VK60     DS    0H                                                               
         LA    R2,MNTSTAH          VALIDATE NETWORK                             
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
*                                                                               
         GOTO1 VALINTWK                                                         
         MVC   SVSTA,BMKTSTA+2                                                  
         MVC   MNTMKN,MKTNM                                                     
         OI    MNTMKNH+6,X'80'                                                  
*                                                                               
VK70     DS    0H                                                               
         LA    R2,MNTDATH          VALIDATE DATE                                
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
*                                                                               
         GOTO1 DATVAL,DMCB,(0,8(R2)),DUB                                        
         CLC   DUB(6),=C'000000'                                                
         BE    INVLFLD                                                          
         GOTO1 DATCON,DMCB,(0,DUB),(2,SVDATE)                                   
         XC    SVDATE,=X'FFFF'      COMPLEMENT THE DATE                         
*                                                                               
VK80     DS    0H                                                               
         LA    R2,MNTMONH          VALIDATE PERIOD                              
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
*                                                                               
         GOTO1 DATVAL,DMCB,(2,8(R2)),DUB                                        
         CLC   DUB(6),=C'000000'                                                
         BE    INVLFLD                                                          
         GOTO1 DATCON,DMCB,DUB,(3,SVMON)                                        
*                                                                               
VK90     DS    0H                                                               
         XC    KEY,KEY                                                          
*                                                                               
         LA    R6,KEY                                                           
         USING NAPRECD,R6                                                       
*                                                                               
         MVI   NAPKTYP,NAPKTYPQ                                                 
         MVI   NAPKSUB,NAPKSUBQ                                                 
         MVC   NAPKDATE,SVDATE                                                  
         MVC   NAPKAGMD,SVMED                                                   
         MVC   NAPKCLT,SVCLT                                                    
         MVC   NAPKPRD,SVPRD                                                    
         MVC   NAPKPTN,SVPTN                                                    
         MVC   NAPKEST,SVEST                                                    
         MVC   NAPKSTA,SVSTA                                                    
         MVC   NAPKMON,SVMON                                                    
*                                                                               
VK100    DS    0H                                                               
         BAS   RE,SETXFIL                                                       
         MVC   AIO,AIO2                                                         
         GOTO1 HIGH                                                             
*                                                                               
         XC    SAVEKEY,SAVEKEY                                                  
         MVC   SAVEKEY(32),KEY                                                  
*                                                                               
VKX      DS    0H                                                               
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE KEY (LIST)                                                           
***********************************************************************         
VKL      DS    0H                                                               
         BAS   RE,SETSFIL                                                       
         XC    SVDATE,SVDATE                                                    
         XC    SVMED,SVMED                                                      
         XC    SVCLT,SVCLT                                                      
         XC    SVPRD,SVPRD                                                      
         XC    SVPTN,SVPTN                                                      
         XC    SVEST,SVEST                                                      
         XC    SVSTA,SVSTA                                                      
*                                                                               
         XC    QPRD,QPRD                                                        
         XC    BPRD,BPRD                                                        
         XC    BEST,BEST                                                        
         XC    BCLT,BCLT                                                        
         XC    BMKTSTA,BMKTSTA                                                  
*                                                                               
         LA    R2,LSTMEDH          VALIDATE MEDIA                               
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         GOTO1 VALIMED                                                          
*                                                                               
         MVC   SVMED,BAGYMD                                                     
         MVC   LSTMDN,MEDNM                                                     
         OI    LSTMDNH+6,X'80'                                                  
*                                                                               
VKL20    DS    0H                                                               
         LA    R2,LSTCLTH          VALIDATE CLIENT                              
         CLI   5(R2),0                                                          
         BE    VKL30                                                            
*                                                                               
         GOTO1 VALICLT                                                          
         MVC   SVCLT,BCLT                                                       
         MVC   LSTCLN,CLTNM                                                     
         OI    LSTCLNH+6,X'80'                                                  
*                                                                               
VKL30    DS    0H                                                               
         LA    R2,LSTPRDH          VALIDATE PRODUCT                             
         CLI   5(R2),0                                                          
         BE    VKL40                                                            
*                                                                               
         GOTO1 VALIPRD                                                          
         MVC   SVPRD,BPRD                                                       
         MVC   LSTPDN,PRDNM                                                     
         OI    LSTPDNH+6,X'80'                                                  
*                                                                               
VKL40    DS    0H                                                               
         LA    R2,LSTPTRH          VALIDATE PRODUCT                             
         CLI   5(R2),0                                                          
         BE    VKL50                                                            
*                                                                               
         GOTO1 VALIPRD                                                          
         MVC   SVPTN,BPRD                                                       
         MVC   LSTPDN,PRDNM                                                     
         OI    LSTPDNH+6,X'80'                                                  
*                                                                               
VKL50    DS    0H                                                               
         LA    R2,LSTESTH          VALIDATE ESTIMATE                            
         CLI   5(R2),0                                                          
         BE    VKL60                                                            
*                                                                               
         ZIC   RE,5(R2)              CONVERT ESTIMATE CODE TO BINARY            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   RE,DUB                                                           
         CHI   RE,1                                                             
         BL    INVLFLD                                                          
         CHI   RE,255                                                           
         BH    INVLFLD                                                          
         STC   RE,BEST               STORE BINARY ESTIMATE                      
         MVC   SVEST,BEST                                                       
*                                                                               
VKL60    DS    0H                                                               
         LA    R2,LSTSTAH          VALIDATE NETWORK                             
         CLI   5(R2),0                                                          
         BE    VKL70                                                            
*                                                                               
         GOTO1 VALINTWK                                                         
         MVC   SVSTA,BMKTSTA+2                                                  
         MVC   LSTMKN,MKTNM                                                     
         OI    LSTMKNH+6,X'80'                                                  
*                                                                               
VKL70    DS    0H                                                               
         LA    R2,LSTDATH          VALIDATE DATE                                
         CLI   5(R2),0                                                          
         BE    VKL80                                                            
*                                                                               
         GOTO1 DATVAL,DMCB,(0,8(R2)),DUB                                        
         CLC   DUB(6),=C'000000'                                                
         BE    INVLFLD                                                          
         GOTO1 DATCON,DMCB,(0,DUB),(2,SVDATE)                                   
         XC    SVDATE,=X'FFFF'      COMPLEMENT THE DATE                         
*                                                                               
VKL80    DS    0H                                                               
*                                                                               
VKL90    DS    0H                                                               
         XC    KEY,KEY                                                          
*                                                                               
         LA    R6,KEY                                                           
         USING NAPRECD,R6                                                       
*                                                                               
         MVI   NAPKTYP,NAPKTYPQ                                                 
         MVI   NAPKSUB,NAPKSUBQ                                                 
         MVC   NAPKDATE,SVDATE                                                  
         MVC   NAPKAGMD,SVMED                                                   
         MVC   NAPKCLT,SVCLT                                                    
         MVC   NAPKPRD,SVPRD                                                    
         MVC   NAPKPTN,SVPTN                                                    
         MVC   NAPKEST,SVEST                                                    
         MVC   NAPKSTA,SVSTA                                                    
*                                                                               
VKL100   DS    0H                                                               
         BAS   RE,SETXFIL                                                       
         MVC   AIO,AIO2                                                         
         GOTO1 HIGH                                                             
*                                                                               
         XC    SAVEKEY,SAVEKEY                                                  
         MVC   SAVEKEY(32),KEY                                                  
*                                                                               
VKLX     DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY KEY                                                                   
***********************************************************************         
DK       DS    0H                                                               
         BAS   RE,SETXFIL                                                       
         L     R6,AIO                                                           
         USING NAPRECD,R6                                                       
*                                                                               
         MVC   MNTMED,NAPMED                                                    
         MVC   MNTCLT,NAPCLT                                                    
         MVC   MNTPRD,NAPPRD                                                    
         MVC   MNTPTR,NAPPRD2                                                   
         CLC   NAPEST,=C'000'                                                   
         BE    *+10                                                             
         MVC   MNTEST,NAPEST                                                    
         MVC   MNTSTA(4),NAPSTA                                                 
         MVC   MNTMON,NAPMONTH                                                  
*                                                                               
         MVC   HALF,NAPKDATE                                                    
         XC    HALF,=X'FFFF'       UNCOMPLIMENT                                 
         GOTO1 DATCON,DMCB,(2,HALF),(10,MNTDAT)                                 
*                                                                               
         OI    MNTMEDH+6,X'80'                                                  
         OI    MNTCLTH+6,X'80'                                                  
         OI    MNTPRDH+6,X'80'                                                  
         OI    MNTPTRH+6,X'80'                                                  
         OI    MNTESTH+6,X'80'                                                  
         OI    MNTSTAH+6,X'80'                                                  
         OI    MNTDATH+6,X'80'                                                  
         OI    MNTMONH+6,X'80'                                                  
*                                                                               
*                                                                               
DKX      DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DELETE RECORD                                                                 
***********************************************************************         
RDEL     DS    0H                                                               
*                                                                               
RDELX    DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE RECORD                                                               
***********************************************************************         
VR       DS    0H                                                               
*                                                                               
VRX      DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY RECORD                                                                
***********************************************************************         
DR       DS    0H                                                               
         BAS   RE,SETXFIL                                                       
*                                                                               
         CLI   ACTNUM,ACTSEL                                                    
         BE    DR10                                                             
*                                                                               
         MVC   AIO,AIO2                                                         
         XC    KEY,KEY                                                          
         MVC   KEY(32),SAVEKEY                                                  
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
*                                                                               
DR10     L     R6,AIO                                                           
         USING NAPRECD,R6                                                       
*                                                                               
         MVC   MNTDME,NAPMED                                                    
         MVC   MNTDCL,NAPCLT                                                    
         MVC   MNTDPR,NAPPRD                                                    
         MVC   MNTDP2,NAPPRD2                                                   
         MVC   MNTDES,NAPEST                                                    
         MVC   MNTDST(4),NAPSTA                                                 
         MVC   MNTDPK,NAPPAK                                                    
         MVC   MNTDPG,NAPPGRP                                                   
         MVC   MNTDMTY,NAPMNTYP                                                 
         MVC   MNTDPT,NAPPTYP                                                   
         MVC   MNTMKT,NAPMKT                                                    
         MVC   MNTSREP,NAPSREP                                                  
         MVC   MNTPAY,NAPPAYER                                                  
         MVC   MNTSUBM,NAPSMED                                                  
*                                                                               
         MVC   MNTDA(4),=C'D/A:'                                                
         GOTO1 HEXOUT,DMCB,KEY+36,MNTDA+4,4,0                                   
*                                                                               
         OC    NAPPAID,NAPPAID                                                  
         BZ    DR50                                                             
         GOTO1 DATCON,DMCB,(2,NAPPAID),(11,MNTPDT)                              
*                                                                               
DR50     DS    0H                                                               
         L     R6,AIO                                                           
         MVC   DATADISP,=H'42'                                                  
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         USING NAPIEL,R6                                                        
*                                                                               
         MVC   MNTINV,NAPIINV                                                   
*                                                                               
         MVC   FULL,NAPIDOLS                                                    
         EDIT  FULL,TEMPDOL,2,ALIGN=LEFT                                        
         MVC   MNTINVG,TEMPDOL                                                  
*                                                                               
         MVC   FULL,NAPINET                                                     
         EDIT  FULL,TEMPDOL,2,ALIGN=LEFT                                        
         MVC   MNTINVN,TEMPDOL                                                  
*                                                                               
         OI    MNTDMEH+6,X'80'                                                  
         OI    MNTDCLH+6,X'80'                                                  
         OI    MNTDPRH+6,X'80'                                                  
         OI    MNTDP2H+6,X'80'                                                  
         OI    MNTDESH+6,X'80'                                                  
         OI    MNTDSTH+6,X'80'                                                  
         OI    MNTDPKH+6,X'80'                                                  
         OI    MNTDPGH+6,X'80'                                                  
         OI    MNTDMTYH+6,X'80'                                                 
         OI    MNTDPTH+6,X'80'                                                  
         OI    MNTMKTH+6,X'80'                                                  
         OI    MNTSREPH+6,X'80'                                                 
         OI    MNTPAYH+6,X'80'                                                  
         OI    MNTPDTH+6,X'80'                                                  
         OI    MNTINVH+6,X'80'                                                  
         OI    MNTSUBMH+6,X'80'                                                 
         OI    MNTINVGH+6,X'80'                                                 
         OI    MNTINVNH+6,X'80'                                                 
         OI    MNTDAH+6,X'80'                                                   
*                                                                               
DRX      DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* LIST RECORDS *                                                                
***********************************************************************         
LR       DS    0H                                                               
         USING LISTRECD,R2                                                      
*                                                                               
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
         CLC   KEY(2),=X'0D3F'                                                  
         BNE   LRX                                                              
*                                                                               
         LA    R6,KEY                                                           
         USING NAPRECD,R6                                                       
*                                                                               
         CLC   NAPKAGMD,SVMED       SAME MEDIA/AGENCY?                          
         BNE   LRSEQ                                                            
*                                                                               
         OC    SVDATE,SVDATE                                                    
         BZ    *+14                                                             
         CLC   NAPKDATE,SVDATE      ANY DATE FILTER?                            
         BNE   LRSEQ                                                            
*                                                                               
         CLI   SVCLT,0              ANY CLIENT FILTER?                          
         BE    *+14                                                             
         CLC   NAPKCLT,SVCLT                                                    
         BNE   LRSEQ                                                            
*                                                                               
         CLI   SVEST,0              ANY ESTIMATE FILTER?                        
         BE    *+14                                                             
         CLC   NAPKEST,SVEST                                                    
         BNE   LRSEQ                                                            
*                                                                               
         CLI   BPRD,0               ANY PRODUCT FILTER?                         
         BE    *+14                                                             
         CLC   NAPKPRD,BPRD                                                     
         BNE   LRSEQ                                                            
*                                                                               
         CLI   SVSTA,0              ANY NETWORK FILTER?                         
         BE    *+14                                                             
         CLC   NAPKSTA,SVSTA                                                    
         BNE   LRSEQ                                                            
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         USING NAPRECD,R6                                                       
*                                                                               
         CLI   QPRD,0               MORE BRANDS PRODUCT?                        
         BE    *+14                                                             
         CLC   NAPPRD,QPRD                                                      
         BNE   LRSEQ                                                            
*                                                                               
         MVC   HALF,NAPKDATE                                                    
         XC    HALF,=X'FFFF'       UNCOMPLIMENT                                 
         GOTO1 DATCON,DMCB,(2,HALF),(10,LDATE)                                  
*                                                                               
         TM    NAPRCNTL,NAPRCPRC    PROCESSED?                                  
         BZ    *+8                                                              
         MVI   LDATE+10,C'*'        SHOW PROCESSED                              
*                                                                               
         MVC   LMED(1),NAPMED                                                   
         CLI   NAPSMED,C' '                                                     
         BE    LR15                                                             
         CLI   NAPSMED,0                                                        
         BE    LR15                                                             
         MVI   LMED+1,C'/'                                                      
         MVC   LMED+2(1),NAPSMED                                                
*                                                                               
LR15     MVC   LCLT,NAPCLT                                                      
         MVC   LPRD,NAPPRD                                                      
         MVC   LPTN,NAPPRD2                                                     
         MVC   LPAK,NAPPAK                                                      
*                                                                               
         CLC   NAPEST,=C'000'                                                   
         BE    *+10                                                             
         MVC   LEST,NAPEST                                                      
*                                                                               
         MVC   LSTA,NAPSTA                                                      
         MVC   LPER,NAPMONTH                                                    
*                                                                               
         OC    NAPPAID,NAPPAID                                                  
         BZ    LR20                                                             
         GOTO1 DATCON,DMCB,(2,NAPPAID),(11,LPAID)                               
         MVI   LDATE+10,C' '       REMOVE ASTERISK TO SHOW PROCESSED            
         DROP  R6                                                               
*                                                                               
LR20     DS    0H                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         USING NAPIEL,R6                                                        
*                                                                               
         MVC   LINV,NAPIINV                                                     
*                                                                               
         GOTO1 LISTMON                                                          
         MVC   LISTAR,SPACES                                                    
         B     LRSEQ                                                            
*                                                                               
LRX      DS    0H                                                               
         B     EXIT                                                             
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
INVLCLI  MVI   ERROR,INVCLI                                                     
         B     TRAPERR                                                          
*                                                                               
INVLACT  MVI   ERROR,INVACT                                                     
         LA    R2,CONACTH                                                       
         B     TRAPERR                                                          
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
******   PRINT OFF                                                              
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
       ++INCLUDE NESFM96D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NESFM95D                                                       
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE NESFMWORKD                                                     
         ORG   SYSSPARE+220                                                     
*                           *******  T31C45 WORK AREA  *******                  
WORKAREA DS    0CL1                                                             
MYDMWRK  DS    12D                                                              
PREVFLAG DS    CL1                                                              
PREVKEY  DS    CL48                                                             
SAVEKEY  DS    CL48                                                             
*                                                                               
*                                                                               
SVDATE   DS    XL2                                                              
SVMED    DS    XL1                                                              
SVCLT    DS    XL2                                                              
SVPRD    DS    XL1                                                              
SVPTN    DS    XL1                                                              
SVEST    DS    XL1                                                              
SVSTA    DS    XL3                                                              
SVMON    DS    XL3                                                              
*                                                                               
TEMPDOL  DS    XL12                                                             
*                                                                               
SYSSW    DS    CL1                                                              
SWDSYS   DS    CL1                                                              
POWCODE  DS    CL2                                                              
ACCOFF   DS    CL2                                                              
OFFLEN   DS    CL1                                                              
COMPCD   DS    CL1                                                              
OLDCOPT2 DS    CL1                                                              
GTFACTB  DS    CL88                                                             
WORKEND  EQU   *                                                                
         EJECT                                                                  
*                                                                               
LIOS     EQU   4000                                                             
*                                                                               
COMHDRD  DSECT                                                                  
       ++INCLUDE SPGENCOM                                                       
COMI2HD  DSECT                                                                  
       ++INCLUDE SPGENXCOM                                                      
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
*                                                                               
       ++INCLUDE SPGENCLG                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPGENAGY                                                       
       ++INCLUDE NEGENAPY                                                       
*                                                                               
PRDRECD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
PRGRECD  DSECT                                                                  
       ++INCLUDE SPGENPRG                                                       
*                                                                               
LISTRECD DSECT                                                                  
LMED     DS    CL3                 MEDIA                                        
         DS    CL1                                                              
LCLT     DS    CL3                 CLIENT                                       
         DS    CL1                                                              
LPRD     DS    CL3                 PRODUCT 1                                    
         DS    CL1                                                              
LPTN     DS    CL3                 PRODUCT 2                                    
         DS    CL1                                                              
LPAK     DS    CL3                 PACKAGE                                      
         DS    CL1                                                              
LEST     DS    CL3                 ESTIMATE                                     
         DS    CL1                                                              
LSTA     DS    CL4                 STATION                                      
         DS    CL1                                                              
LPER     DS    CL6                 PERIOD                                       
         DS    CL1                                                              
LPAID    DS    CL8                 PAID DATE                                    
         DS    CL2                                                              
LINV     DS    CL10                INVOICE                                      
         DS    CL3                                                              
LDATE    DS    CL10                AUTOPAY DATE                                 
*                                                                               
*                                                                               
         PRINT GEN                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'187NESFM53   06/24/08'                                      
         END                                                                    
