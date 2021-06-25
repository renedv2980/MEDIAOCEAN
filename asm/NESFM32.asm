*          DATA SET NESFM32    AT LEVEL 040 AS OF 10/31/05                      
*PHASE T31C32A                                                                  
***********************************************************************         
*                                                                               
*  TITLE: T31C32 - CLIENT/PRODUCT GROUP ASSIGNMENT DISPLAY                      
*                                                                               
*  COMMENTS: SUPPORTS DISPLAY ONLY                                              
*                                                                               
*  CALLED FROM: NET SFM CONTROLLER (T31C00), WHICH CALLS                        
*               DDGENCON (T00A30) WHICH CALLS THIS.                             
*                                                                               
*  CALLS TO:    DATAMGR                                                         
*                                                                               
*  INPUTS: SCREENS NESFME0 (T31CE0) -- CLIENT DISPLAY                           
*                  NESFME4 (T31CE4) -- PRODUCT DISPLAY                          
*                                                                               
*  OUTPUTS: NONE - DISPLAYS CLIENT/PRODUCT GROUP ASSIGNMENTS                    
*                                                                               
*  LOCALS: REGISTER USAGE                                                       
*          R0 - WORK                                                            
*          R1 - WORK                                                            
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CURSOR              
*          R3 - WORK                                                            
*          R4 - WORK                                                            
*          R5 - WORK                                                            
*          R6 - USED FOR GETEL ELEMENT DSECT POINTER                            
*          R7 - WORK                                                            
*          R8 - SPOOLD                                                          
*          R9 - SYSD                                                            
*          RA - TWA                                                             
*          RB - FIRST BASE                                                      
*          RC - GEND                                                            
*          RD - SYSTEM                                                          
*          RE - SYSTEM                                                          
*          RF - SYSTEM                                                          
*                                                                               
***********************************************************************         
         TITLE 'NESFM32 - CLIENT/PRODUCT GROUP ASSIGNMENT DISPLAY'              
T31C32   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T31C32                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T31CFFD,RA                                                       
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         CLI   ACTNUM,ACTDIS       ONLY ACTION DISPLAY ALLOWED                  
         BE    CM                                                               
         LA    R2,CONACTH                                                       
         B     INVLACT                                                          
*                                                                               
CM       CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    DR                                                               
         DC    H'0'                ONLY MODES ALLOWED                           
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE KEY                                                                  
***********************************************************************         
VK       DS    0H                                                               
         XC    SVKEY,SVKEY                                                      
*                                                                               
         CLI   RECNUM,28           PRODUCT REQUEST                              
         BNE   VKCCLI                                                           
         LA    R3,SVKEY                                                         
         USING PRDRECD,R3                                                       
         LA    R2,SFPMEDH          MEDIA                                        
         GOTO1 VALIMED             VALIDATE THE MEDIA CODE                      
         MVC   PKEYAM,BAGYMD                                                    
*                                                                               
         LA    R2,SFPCLIH          CLIENT                                       
         OC    SFPCLI,SPACES                                                    
         GOTO1 VALICLT             VALIDATE THE CLIENT CODE                     
         MVC   PKEYCLT,BCLT                                                     
*                                                                               
         LA    R2,SFPPROH          PRODUCT                                      
         CLC   =C'AAA',SFPPRO      VALIPRD WON'T TAKE THIS                      
         BNE   VK10                                                             
         MVC   PKEYPRD,SFPPRO                                                   
         B     VK20                                                             
*                                                                               
VK10     OC    SFPPRO,SPACES                                                    
         GOTO1 VALIPRD             VALIDATE THE PRODUCT CODE                    
         MVC   PKEYPRD,QPRD                                                     
*                                                                               
VK20     LA    R2,SFPSIDH          START ID                                     
         B     VKID                                                             
         DROP  R3                                                               
*                                                                               
VKCCLI   CLI   RECNUM,27           CLIENT REQUEST                               
         BE    *+6                                                              
         DC    H'0'                ONLY VALUES ALLOWED                          
         LA    R3,SVKEY                                                         
         USING CLTRECD,R3                                                       
*                                                                               
         LA    R2,SFCMEDH          MEDIA                                        
         GOTO1 VALIMED             VALIDATE THE MEDIA CODE                      
         MVC   CKEYAM,BAGYMD                                                    
*                                                                               
         LA    R2,SFCCLIH          CLIENT                                       
         OC    SFCCLI,SPACES                                                    
         GOTO1 VALICLT             VALIDATE THE CLIENT CODE                     
         MVC   CKEYCLT,BCLT                                                     
         LA    R2,SFCSIDH          START ID                                     
         DROP  R3                                                               
*                                                                               
VKID     CLI   5(R2),0             ENTERED?                                     
         BNE   VKA                 YES                                          
         MVI   8(R2),C' '          DEFAULT TO BEGINNING                         
         OI    6(R2),X'80'         XMIT                                         
         B     VKX                 NO - DONE                                    
*                                                                               
VKA      TM    4(R2),X'04'         DATA ALPHABETIC?                             
         BZ    NOTALPH             YES - DONE                                   
*                                                                               
VKX      MVC   KEY,SVKEY                                                        
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY PRODUCT RECORDS                                                       
***********************************************************************         
DR       DS    0H                                                               
         CLI   RECNUM,28           PRODUCT REQUEST                              
         BNE   CDR                                                              
         TWAXC SFPLINH,PROT=Y      CLEAR SCREEN                                 
         L     R6,AIO                                                           
         USING PRDRECD,R6                                                       
         MVC   GRPASS(9),PGRP1     SAVE GROUP ASSIGNMENT CODES                  
         MVC   GRPASS+9(6),PGRP4                                                
         MVC   GRPASS+15(15),PGRP6                                              
         DROP  R6                                                               
         LA    R2,SFPLINH                                                       
         LA    R4,GRPASS                                                        
*                                                                               
DR10     OC    0(3,R4),0(R4)       ANY PRODUCT GRP ASSIGNMENTS??                
         BZ    DR40                                                             
         CLC   SFPSID,0(R4)        FILTER ON START ID                           
         BH    DR40                                                             
         XC    FULL,FULL                                                        
         MVC   FULL(2),1(R4)                                                    
         UNPK  WORK(5),FULL(3)                                                  
*                                                                               
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING PRGRECD,R3                                                       
         MVC   PRGKTYP,=X'0D01'    FIND PRODUCT GROUPS                          
         MVC   PRGKAGMD,SVKEY+1                                                 
         MVC   PRGKCLT,SVKEY+2                                                  
         MVC   PRGKID,0(R4)        GET DEFINITION RECORD FIRST                  
         GOTO1 MYHIGH                                                           
*                                                                               
         MVC   LISTAR,SPACES                                                    
         MVC   LSTID,PRGKID                                                     
         CLC   KEY(L'PRGKEY),KEYSAVE                                            
         BE    DR20                                                             
*                                                                               
DR15     MVC   LSTCODE(17),=C'*** NOT FOUND ***'                                
         B     DR30                                                             
*                                                                               
DR20     DS    0H                                                               
         GOTO1 MYGETREC                                                         
         MVC   DATADISP,=H'24'                                                  
         L     R6,AIO                                                           
         USING PRGEL01,R6                                                       
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   LSTBRK1,PRGBK1                                                   
         MVC   LSTBRK2,PRGBK2                                                   
         ZIC   R0,PRGBK1LN                                                      
         ZIC   R1,PRGBK2LN                                                      
         AR    R1,R0                                                            
         DROP  R6                                                               
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LSTCODE(0),WORK                                                  
*                                                                               
         MVC   PRGKGRP,1(R4)                                                    
         GOTO1 MYHIGH                                                           
         CLC   KEY(L'PRGKEY),KEYSAVE                                            
         BNE   DR15                                                             
*                                                                               
         GOTO1 MYGETREC                                                         
         MVC   DATADISP,=H'24'                                                  
         L     R6,AIO                                                           
         USING PRGEL10,R6                                                       
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   LSTNAME1,PRGNAM1                                                 
         MVC   LSTNAME2,PRGNAM2                                                 
         DROP  R6                                                               
*                                                                               
DR30     MVC   8(79,R2),LISTAR     TO SCREEN LINE                               
         OI    6(R2),X'80'         XMIT                                         
         ZIC   R0,0(R2)            NEXT FIELD                                   
         AR    R2,R0                                                            
         LA    RF,SFPLSTH          LAST                                         
         CR    R2,RF               END OF SCREEN ?                              
         BH    DRX                 YES                                          
*                                                                               
DR40     LA    R4,3(R4)                                                         
         LA    R1,GRPASS+30                                                     
         CR    R4,R1                                                            
         BL    DR10                                                             
*                                                                               
DRX      B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY CLIENT RECORDS                                                        
***********************************************************************         
CDR      DS    0H                                                               
         CLI   RECNUM,27           MUST BE CLIENT RECORD                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TWAXC SFCLINH,PROT=Y      CLEAR SCREEN                                 
         L     R6,AIO                                                           
         USING CLTRECD,R6                                                       
         MVC   GRPASS,CGRP1        SAVE GROUP ASSIGNMENT CODES                  
         MVC   GRPASS+15(15),CGRP6                                              
         DROP  R6                                                               
         LA    R2,SFCLINH                                                       
         LA    R4,GRPASS                                                        
*                                                                               
CDR10    OC    0(3,R4),0(R4)       ANY PRODUCT GRP ASSIGNMENTS??                
         BZ    CDR40                                                            
         CLC   SFCSID,0(R4)        FILTER ON START ID                           
         BH    CDR40                                                            
         XC    FULL,FULL                                                        
         MVC   FULL(2),1(R4)                                                    
         UNPK  WORK(5),FULL(3)                                                  
*                                                                               
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING CLGRECD,R3                                                       
         MVC   CLGKTYP,=X'0D06'    FIND CLIENT GROUPS                           
         MVC   CLGKAGMD,SVKEY+1                                                 
         MVC   CLGKID,0(R4)        GET DEFINITION RECORD FIRST                  
         GOTO1 MYHIGH                                                           
*                                                                               
         MVC   LISTAR,SPACES                                                    
*                                                                               
         MVC   BYTE,CLGKID                                                      
         BAS   RE,TRANS12                                                       
         MVC   LSTID,DUB                                                        
         MVI   BYTE,0                                                           
*                                                                               
         CLC   KEY(L'CLGKEY),KEYSAVE                                            
         BE    CDR20                                                            
*                                                                               
CDR15    MVC   LSTCODE(17),=C'*** NOT FOUND ***'                                
         B     CDR30                                                            
*                                                                               
CDR20    DS    0H                                                               
         GOTO1 MYGETREC                                                         
         MVC   DATADISP,=H'24'                                                  
         L     R6,AIO                                                           
         USING CLGDESD,R6                                                       
         MVI   ELCODE,CLGDESQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   LSTBRK1,CLGBK1                                                   
         MVC   LSTBRK2,CLGBK2                                                   
         ZIC   R0,CLGBK1LN                                                      
         ZIC   R1,CLGBK2LN                                                      
         AR    R1,R0                                                            
         DROP  R6                                                               
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LSTCODE(0),WORK                                                  
*                                                                               
         MVC   CLGKGRP,1(R4)                                                    
         GOTO1 MYHIGH                                                           
         CLC   KEY(L'CLGKEY),KEYSAVE                                            
         BNE   CDR15                                                            
*                                                                               
         GOTO1 MYGETREC                                                         
         MVC   DATADISP,=H'24'                                                  
         L     R6,AIO                                                           
         USING CLGMEMD,R6                                                       
         MVI   ELCODE,CLGMEMQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   LSTNAME1,CLGNAM1                                                 
         MVC   LSTNAME2,CLGNAM2                                                 
         DROP  R6                                                               
*                                                                               
CDR30    MVC   8(79,R2),LISTAR     TO SCREEN LINE                               
         OI    6(R2),X'80'         XMIT                                         
         ZIC   R0,0(R2)            NEXT FIELD                                   
         AR    R2,R0                                                            
         LA    RF,SFCLSTH          LAST                                         
         CR    R2,RF               END OF SCREEN ?                              
         BH    CDRX                YES                                          
*                                                                               
CDR40    LA    R4,3(R4)                                                         
         LA    R1,GRPASS+30                                                     
         CR    R4,R1                                                            
         BL    CDR10                                                            
*                                                                               
CDRX     B     EXIT                                                             
*                                                                               
TRANS12  DS    0H                                                               
         XC    DUB,DUB                                                          
         LA    R1,SPCGRTAB                                                      
         LHI   R0,(SPCGRTBX-SPCGRTAB)/3                                         
*                                                                               
TRANS12A CLC   BYTE,2(R1)                                                       
         BNE   *+12                                                             
         MVC   DUB(2),0(R1)                                                     
         BR    RE                                                               
*                                                                               
         LA    R1,3(R1)                                                         
         BCT   R0,TRANS12A                                                      
         B     INVID                                                            
***********************************************************************         
* ERROR MESSAGES                                                                
***********************************************************************         
INVLFLD  MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
NOTALPH  MVI   ERROR,NOTALPHA                                                   
         B     TRAPERR                                                          
*                                                                               
INVLACT  MVI   ERROR,INVACT                                                     
         B     TRAPERR                                                          
*                                                                               
MISSFLD  MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
INVID    XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(35),=C'*** INVALID GROUP ID               '              
         B     MYERR                                                            
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
         B     EXIT                                                             
*                                                                               
MYERR    GOTO1 ERREX2                                                           
         B     EXIT                                                             
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
***********************************************************************         
* DATAMGR CALLS                                                                 
***********************************************************************         
MYHIGH   NTR1                                                                   
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(BYTE,=C'DMRDHI'),=CL8'SPTDIR',KEY,KEY,0            
         BAS   RE,DMCHECK                                                       
         B     YES                                                              
*                                                                               
MYGETREC NTR1                                                                   
         GOTO1 DATAMGR,DMCB,(BYTE,=C'GETREC'),=C'SPTFILE ',            X        
               KEY+14,AIO,MYDMWRK                                               
         BAS   RE,DMCHECK                                                       
         B     YES                                                              
*                                                                               
DMCHECK  CLI   8(R1),0                                                          
         BER   RE                                                               
         TM    8(R1),X'92'                                                      
         BM    NO                                                               
         DC    H'0'                                                             
*                                                                               
YES      SR    R1,R1                                                            
         B     *+8                                                              
NO       LA    R1,1                                                             
         LTR   R1,R1                                                            
         XIT1  REGS=(R0,R1)                                                     
         EJECT                                                                  
         LTORG                                                                  
       ++INCLUDE SPCGRTAB                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE NESFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NESFME0D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NESFME4D                                                       
         EJECT                                                                  
       ++INCLUDE NESFMWORKD                                                     
         ORG   SYSSPARE                                                         
         PRINT ON                                                               
*                           *******  T31C32 WORK AREA  *******                  
WORKAREA DS    0CL1                                                             
IDLEN    DS    CL1                                                              
GRPASS   DS    CL30                                                             
         DS    0D                                                               
MYDMWRK  DS    CL96                                                             
WORKEND  EQU   *                                                                
*                                                                               
GEND     DSECT                   SCREEN LINE WORK AREA                          
         ORG   LISTAR             (LISTMON NOT USED)                            
LSTID    DS    CL2                                                              
         DS    CL1                                                              
LSTCODE  DS    CL4                                                              
         DS    CL2                                                              
LSTBRK1  DS    CL12                                                             
         DS    CL2                                                              
LSTNAME1 DS    CL20                                                             
         DS    CL2                                                              
LSTBRK2  DS    CL12                                                             
         DS    CL2                                                              
LSTNAME2 DS    CL20                                                             
*                                                                               
         EJECT                                                                  
CLTRECD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENCLG                                                       
PRDRECD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENPRG                                                       
 END                                                                            
