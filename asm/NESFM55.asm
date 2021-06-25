*          DATA SET NESFM55    AT LEVEL 084 AS OF 10/22/09                      
*PHASE T31C55A                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        T31C55  -- NET BILLING HOLD FILE                     *         
*                                                                     *         
*  COMMENTS:     MAINTAINS BILLING HOLD RECS ON XSPFILE               *         
*                                                                     *         
*  CALLED FROM:  SFM CONTROLLER (T31C00), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       SCREENS NESFM98(MAINT), AND NESFM99(LIST)            *         
*                                                                     *         
*  OUTPUTS:      CURRENT BILLING HOLD RECORDS                         *         
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
         TITLE 'T31C55 - BILLING HOLD MAINTENANCE'                              
T31C55   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1C55**,R7,RR=R3                                              
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
         GOTO1 VTERMACC            CHECK FOR DISP/LIST ONLY TERMINALS           
*                                                                               
         BRAS  RE,SETUP                                                         
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,SETFILE        SET FILE                                     
         BE    SF                                                               
*                                                                               
EQXIT    CR    RB,RB                                                            
         J     XIT                                                              
NEQXIT   LTR   RB,RB                                                            
XIT      XIT1                                                                   
*                                                                               
MAXPRD   EQU   252                                                              
*                                                                               
***********************************************************************         
*                       SET FILE                                      *         
***********************************************************************         
*                                                                               
SF       DS    0H                                                               
         BRAS  RE,SSV                                                           
         B     XIT                                                              
*                                                                               
***********************************************************************         
*                       VALIDATE KEY                                  *         
***********************************************************************         
*                                                                               
VK       DS    0H                                                               
         XC    SVFLDS(SVFLDSLQ),SVFLDS                                          
         XC    SAVEKEY,SAVEKEY                                                  
*                                                                               
* CLEAR CLIENT, PRODUCT, AND ESTIMATE NAME FIELDS                               
*                                                                               
         MVC   BHNCNM,SPACES                                                    
         OI    BHNCNMH+6,X'80'                                                  
         MVC   BHNPNM,SPACES                                                    
         OI    BHNPNMH+6,X'80'                                                  
         MVC   BHNENM,SPACES                                                    
         OI    BHNENMH+6,X'80'                                                  
*                                                                               
         BRAS  RE,RSV                                                           
*                                                                               
***********************************************************************         
*                                                                               
         XC    FAKEFLD,FAKEFLD                                                  
         MVI   FAKEFLD,9                                                        
         MVI   FAKEFLD+8,C'N'                                                   
         LA    R2,FAKEFLD          MEDIA                                        
         MVI   BYTE,C'A'                                                        
         MVC   AIO,AIO1                                                         
         GOTO1 VALIMED             VALIDATE MEDIA CODE AND TRANSMIT             
         MVC   SVAM,BAGYMD                                                      
*                                                                               
***********************************************************************         
*                                                                               
         LA    R2,BHNCLTH            CLIENT                                     
         CLI   5(R2),0                                                          
         BNE   *+16                                                             
         CLI   ACTNUM,ACTLIST        FIELD CAN BE BLANK IF ACTION=LIST          
         BE    VK03                  PROCEED TO PRODUCT                         
         B     ERRMIS                                                           
*                                                                               
         MVI   BYTE,C'A'                                                        
         MVC   AIO,AIO1                                                         
         GOTO1 VALICLT               VALIDATE CLIENT CODE AND TRANSMIT          
*                                                                               
         MVC   BHNCNM,CLTNM          CLIENT NAME                                
         OI    BHNCNMH+6,X'80'                                                  
         MVC   SVBCLT,BCLT                                                      
*                                                                               
***********************************************************************         
*                                                                               
VK03     DS    0H                                                               
         LA    R2,BHNPRDH            PRODUCT                                    
         CLI   5(R2),0                                                          
         BNE   *+16                                                             
         CLI   ACTNUM,ACTLIST        FIELD CAN BE BLANK IF ACTION=LIST          
         BE    VK10                  PROCEED TO ESTIMATE                        
         B     ERRMIS                                                           
*                                                                               
         CLI   BHNCLTH+5,0           SEE IF CLIENT ENTERED                      
         BNE   *+12                                                             
         LA    R2,BHNCLTH                                                       
         B     ERRMIS                                                           
*                                                                               
         CLI   5(R2),3                                                          
         BNE   *+14                                                             
         CLC   8(3,R2),=C'POL'                                                  
         BE    ERRINV                                                           
*                                                                               
         MVI   BYTE,C'A'                                                        
         MVC   AIO,AIO1                                                         
         GOTO1 VALIPRD                                                          
         L     R1,AIO                                                           
         USING PRDHDR,R1                                                        
         MVC   BHNPNM,PNAME          VALIDATE PRODUCT CODE AND TRANSMIT         
         DROP  R1                                                               
         OI    BHNPNMH+6,X'80'       PRODUCT NAME                               
         MVC   SVPRD,QPRD                                                       
*                                                                               
***********************************************************************         
*                                                                               
VK10     DS    0H                                                               
         LA    R2,BHNESTH            ESTIMATE                                   
*                                                                               
         CLI   5(R2),0                                                          
         BNE   *+16                                                             
         CLI   ACTNUM,ACTLIST        FIELD CAN BE BLANK IF ACTION=LIST          
         BE    VK30                  PROCEED TO NETWORK                         
         B     ERRMIS                                                           
*                                                                               
         CLI   BHNPRDH+5,0           SEE IF PRODUCT ENTERED                     
         BNE   *+12                                                             
         LA    R2,BHNPRDH                                                       
         B     ERRMIS                                                           
*                                                                               
         MVI   BYTE,C'A'                                                        
         MVC   AIO,AIO1                                                         
         GOTO1 VALIEST                                                          
         MVC   BHNENM,ESTNAME                                                   
         OI    BHNENMH+6,X'80'                                                  
         MVC   SVEST,BEST                                                       
         L     R1,AIO                                                           
         USING ESTHDR,R1                                                        
         MVC   SVESTART,ESTART                                                  
         MVC   SVEEND,EEND                                                      
         DROP  R1                                                               
*                                                                               
**********************************************************************          
*                                                                               
VK30     DS    0H                  NETWORK                                      
         LA    R2,BHNNETH                                                       
         CLI   5(R2),0                                                          
         BNE   *+16                                                             
         CLI   ACTNUM,ACTLIST        FIELD CAN BE BLANK IF ACTION=LIST          
         BE    VK50                                                             
         B     ERRMIS                                                           
*                                                                               
         MVI   BYTE,C'A'                                                        
         MVC   AIO,AIO1                                                         
         GOTO1 VALINTWK                                                         
         MVC   SVNET,QNET                                                       
*                                                                               
***********************************************************************         
*                                                                               
VK50     DS    0H                  MOS                                          
         LA    R2,BHSMOSH                                                       
         CLI   5(R2),0                                                          
         BNE   *+16                                                             
         CLI   ACTNUM,ACTLIST        FIELD CAN BE BLANK IF ACTION=LIST          
         BE    VK70                                                             
         B     ERRMIS                                                           
*                                                                               
         BRAS  RE,VMOS             THIS ROUTINE SETS VALUE OF SVMOS             
         BNE   ERRINV                                                           
*                                                                               
***********************************************************************         
*                                                                               
VK70     DS    0H                  PACKAGE                                      
         LA    R2,BHSPKGH                                                       
         CLI   5(R2),0                                                          
         BNE   *+16                                                             
         CLI   ACTNUM,ACTLIST        FIELD CAN BE BLANK IF ACTION=LIST          
         BE    VK100                                                            
         B     ERRMIS                                                           
*                                                                               
         BRAS  RE,VPKG                                                          
         BNE   ERRINV                                                           
*                                                                               
VK100    DS    0H                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    VK101                                                            
         CLI   ACTNUM,ACTREST                                                   
         BNE   VKX                                                              
*                                                                               
VK101    DS    0H                                                               
         LA    R2,BHSMOSH                                                       
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING BHLNRECD,R3                                                      
*                                                                               
         MVI   BHLNKSYS,BHLNKSYQ                                                
         MVI   BHLNKSTP,BHLNKSTQ                                                
         MVC   BHLNKAM,SVAM                                                     
         MVC   BHLNKCLI,SVBCLT                                                  
         MVC   BHLNKPRD,SVPRD                                                   
         MVC   BHLNKEST,SVEST                                                   
         MVC   BHLNKNET,SVNET                                                   
*                                                                               
         XC    SAVEKEY,SAVEKEY                                                  
         MVC   SAVEKEY,KEY                                                      
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(BHLNKMOS-BHLNKEY),SAVEKEY SAME A/M THROUGH NET?              
         BNE   VK110                                                            
         OC    SVMOS,SVMOS                                                      
         BZ    *+18                                                             
         OC    BHLNKMOS,BHLNKMOS                                                
         BZ    VKERR01                                                          
         B     VK110                                                            
*                                                                               
         OC    BHLNKMOS,BHLNKMOS                                                
         BNZ   VKERR01                                                          
*                                                                               
VK110    DS    0H                                                               
         LA    R2,BHSPKGH                                                       
         XC    KEY,KEY                                                          
         MVC   KEY,SAVEKEY                                                      
         MVC   BHLNKMOS,SVMOS                                                   
         MVC   SAVEKEY,KEY                                                      
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(BHLNKPKG-BHLNKEY),SAVEKEY SAME A/M THROUGH MOS?              
         BNE   VKX                                                              
         OC    SVPKG,SVPKG                                                      
         BZ    *+18                                                             
         OC    BHLNKPKG,BHLNKPKG                                                
         BZ    VKERR03                                                          
         B     VKX                                                              
*                                                                               
         OC    BHLNKPKG,BHLNKPKG                                                
         BNZ   VKERR04                                                          
*                                                                               
         DROP  R3                                                               
*                                                                               
VKX      DS    0H                                                               
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING BHLNRECD,R3                                                      
*                                                                               
         MVI   BHLNKSYS,BHLNKSYQ                                                
         MVI   BHLNKSTP,BHLNKSTQ                                                
         MVC   BHLNKAM,SVAM                                                     
         MVC   BHLNKCLI,SVBCLT                                                  
         MVC   BHLNKPRD,SVPRD                                                   
         MVC   BHLNKEST,SVEST                                                   
         MVC   BHLNKNET,SVNET                                                   
         MVC   BHLNKMOS,SVMOS                                                   
         MVC   BHLNKPKG,SVPKG                                                   
         DROP  R3                                                               
         BRAS  RE,SSV                                                           
         MVC   AIO,AIO1                                                         
         B     XIT                                                              
*                                                                               
* VALKEY ERRORS                                                                 
*                                                                               
VKERR01  DS    0H                  ALL-MONTH BHOLD EXISTS                       
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(40),=CL40'ERROR: ALL-MONTH BHOLD EXISTS'                 
         GOTO1 ERREX2                                                           
*                                                                               
VKERR02  DS    0H                  MONTH-SPECIFIC BHOLD EXISTS                  
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(40),=CL40'ERROR: MONTH-SPECIFIC BHOLD EXISTS'            
         GOTO1 ERREX2                                                           
*                                                                               
VKERR03  DS    0H                  ALL-PKG BHOLD EXISTS                         
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(40),=CL40'ERROR: ALL-PACKAGE BHOLD EXISTS'               
         GOTO1 ERREX2                                                           
*                                                                               
VKERR04  DS    0H                  PKG-SPECIFIC BHOLD EXISTS                    
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(40),=CL40'ERROR: PACKAGE-SPECIFIC BHOLD EXISTS'          
         GOTO1 ERREX2                                                           
*                                                                               
***********************************************************************         
*                       DISPLAY KEY                                   *         
***********************************************************************         
DK       DS    0H                                                               
         XC    SAVEKEY,SAVEKEY                                                  
         MVC   SAVEKEY,KEY                                                      
*                                                                               
         L     R6,AIO                                                           
         USING BHLNRECD,R6                                                      
         MVC   SVAM,BHLNKAM                                                     
*                                                                               
**********************************************************************          
*                                                                               
         LA    R1,BHLNKCLI                                                      
         MVC   SVBCLT,0(R1)                                                     
*                                                                               
         BRAS  RE,DISBCLT                                                       
         MVC   BHNCLT,WORK                                                      
         OI    BHNCLTH+6,X'80'                                                  
         MVC   BHNCNM,WORK+3                                                    
         OI    BHNCNMH+6,X'80'                                                  
*                                                                               
**********************************************************************          
*                                                                               
         LA    R1,BHLNKPRD                                                      
         MVC   SVPRD,0(R1)                                                      
*                                                                               
         BRAS  RE,DISPRD                                                        
         MVC   BHNPRD,BHLNKPRD                                                  
         OI    BHNPRDH+6,X'80'                                                  
         MVC   BHNPNM,WORK+3                                                    
         OI    BHNPNMH+6,X'80'                                                  
*                                                                               
**********************************************************************          
*                                                                               
         LA    R1,BHLNKEST                                                      
         MVC   SVEST,0(R1)                                                      
*                                                                               
         BRAS  RE,DISEST                                                        
         MVC   BHNEST,WORK                                                      
         OI    BHNESTH+6,X'80'                                                  
         MVC   BHNENM,WORK+3                                                    
         OI    BHNENMH+6,X'80'                                                  
*                                                                               
**********************************************************************          
*                                                                               
         MVC   BHNNET,BHLNKNET                                                  
         OI    BHNNETH+6,X'80'                                                  
*        MVC   SVNET,BHNNET                                                     
*                                                                               
**********************************************************************          
*                                                                               
         LA    R3,BHLNKMOS                                                      
         MVC   SVMOS,0(R3)                                                      
*                                                                               
         MVC   BHSMOS(3),=C'ALL'                                                
         OI    BHSMOSH+6,X'80'                                                  
         OC    0(L'BHLNKMOS,R3),0(R3)                                           
         BZ    DK50                                                             
         GOTO1 DATCON,DMCB,(3,0(R3)),(6,BHSMOS)                                 
*                                                                               
**********************************************************************          
*                                                                               
DK50     DS    0H                                                               
         LA    R3,BHLNKPKG                                                      
         MVC   SVPKG,0(R3)                                                      
*                                                                               
         MVC   BHSPKG,=C'ALL'                                                   
         OI    BHSPKGH+6,X'80'                                                  
         OC    0(L'BHLNKPKG,R3),0(R3)                                           
         BZ    DKX                                                              
         EDIT  (B1,0(R3)),BHSPKG,ALIGN=LEFT                                     
*                                                                               
DKX      DS    0H                                                               
         BRAS  RE,SSV                                                           
         XC    KEY,KEY                                                          
         MVC   KEY(L'BHLNKEY),SAVEKEY                                           
         GOTO1 HIGH                                                             
         CLC   KEY(L'BHLNKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
*                                                                               
         B     XIT                                                              
         DROP  R6                                                               
*                                                                               
***********************************************************************         
*                       DISPLAY RECORD                                *         
***********************************************************************         
DR       DS    0H                                                               
         XC    SAVEKEY,SAVEKEY                                                  
         BRAS  RE,CLR                                                           
*                                                                               
         BRAS  RE,SSV                                                           
*                                                                               
         L     R6,AIO                                                           
         CLI   ACTNUM,ACTSEL                                                    
         BNE   *+14                                                             
         MVC   PREVKEY,0(R6)                                                    
         MVI   PREVFLAG,C'Y'                                                    
*                                                                               
         MVI   ELCODE,HLDELCDQ                                                  
         MVC   DATADISP,=AL2(42)                                                
         BRAS  RE,GETEL                                                         
         BNE   DRX                                                              
*                                                                               
         USING  HLDELD,R6                                                       
*                                                                               
         OC    HLDELDAT,HLDELDAT                                                
         BZ    DRX                                                              
         GOTO1 DATCON,DMCB,(2,HLDELDAT),(11,BHNEDT)                             
         OI    BHNEDTH+6,X'80'                                                  
         DROP  R6                                                               
*                                                                               
DRX      DS    0H                                                               
         L     R6,AIO                                                           
         USING BHLNRECD,R6                                                      
         MVC   SVAM,BHLNKAM                                                     
         MVC   SVBCLT,BHLNKCLI                                                  
         MVC   SVPRD,BHLNKPRD                                                   
         MVC   SVEST,BHLNKEST                                                   
         MVC   SVNET,BHLNKNET                                                   
         MVC   SVMOS,BHLNKMOS                                                   
         MVC   SVPKG,BHLNKPKG                                                   
*                                                                               
         B     XIT                                                              
         DROP  R6                                                               
*                                                                               
***********************************************************************         
*                       VALIDATE RECORD                               *         
***********************************************************************         
*                                                                               
VR       DS    0H                                                               
         XC    SAVEKEY,SAVEKEY                                                  
         BRAS  RE,SSV                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,HLDELCDQ     GET RID OF MAIN ELEMENT                      
         GOTO1 REMELEM                                                          
*                                                                               
         XC    ELEM,ELEM                                                        
         USING HLDELD,R6                                                        
         LA    R6,ELEM                                                          
         MVI   HLDELCOD,HLDELCDQ   ELEMENT CODE                                 
         MVI   HLDELLEN,HLDELLNQ   ELEMENT LENGTH                               
*                                                                               
         BRAS  RE,RSV                                                           
*                                                                               
         LA    R2,BHNEDTH          A(EFFECTIVE DATE FIELD)                      
         CLI   5(R2),0             TEST ANY DATA                                
         BE    ERRMIS                                                           
*                                                                               
         XC    CHARYMD,CHARYMD                                                  
         GOTO1 DATVAL,DMCB,(0,8(R2)),CHARYMD                                    
         OC    DMCB,DMCB           TEST VALID M/D/Y                             
         BZ    ERRINV              YES                                          
         GOTO1 DATCON,DMCB,(0,CHARYMD),(2,HLDELDAT)                             
*&&DO                                                                           
         GOTO1 DATCON,DMCB,(0,CHARYMD),(0,WORK)                                 
         CLC   SVESTART,WORK                                                    
         BH    ERRINV                                                           
         CLC   SVEEND,WORK                                                      
         BL    ERRINV                                                           
*&&                                                                             
*                                                                               
         BRAS  RE,SSV                                                           
         XC    KEY,KEY                                                          
         OC    SAVEKEY,SAVEKEY                                                  
         BZ    VR100                                                            
*                                                                               
         MVC   KEY(L'BHLNKEY),SAVEKEY                                           
*                                                                               
VR100    DS    0H                                                               
         MVC   AIO,AIO1                                                         
         GOTO1 ADDELEM                                                          
*                                                                               
VRX      DS    0H                                                               
         OI    GENSTAT2,RETEQSEL     PUT IT AND REDISPLAY                       
         B     DR                                                               
         DROP  R6                                                               
*                                                                               
***********************************************************************         
*                       LIST RECORDS                                  *         
***********************************************************************         
*                                                                               
LR       DS    0H                                                               
         BRAS  RE,SSV                                                           
*                                                                               
         CLI   PREVFLAG,C'Y'                                                    
         BNE   LR05                                                             
         XC    KEY,KEY                                                          
         MVC   KEY(L'BHLNKEY),PREVKEY                                           
         MVI   PREVFLAG,C'N'                                                    
         B     LR09                                                             
*                                                                               
LR05     DS    0H                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING BHLNRECD,R6                                                      
         MVI   BHLNKSYS,BHLNKSYQ                                                
         MVI   BHLNKSTP,BHLNKSTQ                                                
         MVC   BHLNKAM,SVAM                                                     
         MVC   BHLNKCLI,SVBCLT                                                  
         MVC   BHLNKPRD,SVPRD                                                   
         MVC   BHLNKEST,SVEST                                                   
*                                                                               
LR09     MVC   SAVEKEY,KEY                                                      
*                                                                               
LR10     GOTO1 HIGH                                                             
         B     LR20                                                             
*                                                                               
LR15     GOTO1 SEQ                                                              
*                                                                               
LR20     DS    0H                                                               
         CLC   KEY(3),SAVEKEY                                                   
         BNE   LRX                                                              
*                                                                               
         MVC   SAVEKEY,KEY                                                      
         BAS   RE,FILTERS                                                       
         BNE   LR15                      YES                                    
*                                                                               
         GOTO1 GETREC                                                           
         MVC   LISTAR,SPACES                                                    
*                                                                               
         L     R6,AIO                                                           
         USING BHLNRECD,R6                                                      
*                                                                               
         LA    R1,BHLNKCLI                                                      
         BRAS  RE,DISBCLT                                                       
*                                                                               
         MVC   AIO,AIO1                                                         
         BRAS  RE,SSV                                                           
         L     R6,AIO                                                           
         MVC   LSTCLT,WORK                                                      
*                                                                               
         MVC   LSTPRD,BHLNKPRD                                                  
*                                                                               
         EDIT  BHLNKEST,LSTEST                                                  
*                                                                               
         MVC   LSTNET,BHLNKNET                                                  
*                                                                               
         MVC   LSTMOS(6),=CL6'ALL'                                              
         OC    BHLNKMOS,BHLNKMOS                                                
         BZ    LR30                                                             
         GOTO1 DATCON,DMCB,(3,BHLNKMOS),(6,LSTMOS)                              
*                                                                               
LR30     DS    0H                                                               
         MVC   LSTPKG,=C'ALL'                                                   
         CLI   BHLNKPKG,0                                                       
         BE    LR40                                                             
         EDIT  BHLNKPKG,LSTPKG                                                  
*                                                                               
LR40     DS    0H                                                               
         MVI   ELCODE,HLDELCDQ                                                  
         MVC   DATADISP,=AL2(42)                                                
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING HLDELD,R6                                                        
         OC    HLDELDAT,HLDELDAT                                                
         BZ    LR50                                                             
         GOTO1 DATCON,DMCB,(2,HLDELDAT),(11,LSTEDATE)                           
*                                                                               
LR50     DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'BHLNKEY),SAVEKEY                                           
         GOTO1 HIGH                                                             
         CLC   KEY(L'BHLNKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
*                                                                               
         GOTO1 LISTMON                                                          
*                                                                               
         MVC   PREVKEY,KEY                                                      
         MVI   PREVFLAG,C'Y'                                                    
*                                                                               
         B     LR15                                                             
*                                                                               
LRX      DS    0H                                                               
         MVI   PREVFLAG,C'N'                                                    
         B     XIT                                                              
         DROP  R6                                                               
*                                                                               
*                                                                               
***********************************************************************         
*                          ERROR MESSAGES                             *         
***********************************************************************         
*                                                                               
ERRINV   MVI   ERROR,INVALID                                                    
         B     VSFMERR                                                          
ERRMIS   MVI   ERROR,MISSING                                                    
         B     VSFMERR                                                          
ERRSEC2  MVC   ERRNUM,=AL2(NOTAUTH)                                             
         B     SPERREX                                                          
ERRBKLN  MVC   ERRNUM,=AL2(BKLNINV)                                             
         B     SPERREX                                                          
ERRBRPOL MVC   ERRNUM,=AL2(BRPOLER1)                                            
         B     SPERREX                                                          
ERRDMPOL MVC   ERRNUM,=AL2(DEMERR2)                                             
         B     SPERREX                                                          
ERRNOCHG MVC   ERRNUM,=AL2(CHGERR)                                              
         B     SPERREX                                                          
NODEMDEL MVC   ERRNUM,=AL2(NODEMODE)                                            
         B     SPERREX                                                          
ERREXC   MVC   ERRNUM,=AL2(DEMOEXC)                                             
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
*                                                                               
*        ERRORMESSAGES                                                          
*                                  SHORT DESP OF ERROR MSGS                     
NUMERR   EQU   3                   NUMERIC ONLY                                 
ALPHAERR EQU   4                   ALPHABETIC ONLY                              
NOTAUTH  EQU   175                 NOT AUTHRORIZED FOR THIS FUNCTION            
BKLNINV  EQU   449                 BREAK LN MUST BE 1-4                         
GMIERR   EQU   486                 GMI NOT SET UP FOR CLIENT                    
BIGERR   EQU   487                 OTHER AGENCY FEE TOO LARGE                   
TALERR   EQU   488                 NTP NOT VALID FOR PRODUCT                    
TALREQ   EQU   489                 NTP REQUIRED FOR THIS CLIENT                 
CLTFULL  EQU   485                 TOO MANY PRODUCTS                            
PRDERR   EQU   491                 PRD ERROR CHECK TRAFFIC MASTER PRD           
SCRTYERR EQU   492                 ACCESS TO CLIENT NOT AUTHORIZED              
ERRTRADE EQU   493                 TRADE PRD CODE ALREAD IN CLIST               
ERRTRAD2 EQU   494                 THIRD CHAR IN PRD CODE MUST BE 'C'           
CLNTERR  EQU   40                  CLIENT NOT FOUND                             
PRODERR  EQU   41                  PRODUCT NOT FOUND                            
NTPERR   EQU   495                 NTP MUST FIT BETWEEN 0 AND 2                 
BADDATE  EQU   20                  INVALID DATE                                 
PNREQER  EQU   496                 PRD NAME REQUIRED                            
PNDELER  EQU   497                 PRD NAME CANNOT BE DELETE                    
CPERR1   EQU   498                 CLT/PRD CODE REQUIRED                        
CPERR2   EQU   499                 CLT/PRD CODE MUST BE 4 ALPHANUMER            
CPERR3   EQU   503                 CLT/PRD CODE MUST BE 4 NUMERICS              
CPERR4   EQU   505                 CLT/PRD CODE MUST BE 5 NUMERICS              
PCLERR1  EQU   506                 CLASSES MUST BE A - I                        
PCLERR2  EQU   507                 CLASSES CANNOT BE EQUAL                      
BTNERR   EQU   508                 BILL-TO-NAME REQUIRED                        
OAFERR   EQU   509                 OAF CANNOT EXCEED 9.99                       
BBERR    EQU   510                 BILL-BASIS IS CGROSS OR CNET                 
COMPERR1 EQU   511                 COM % REQUIRED                               
COMPERR2 EQU   515                 COM % VALID NUMERIC                          
COMPERR3 EQU   512                 100% IS MAX COM %                            
COMPERR4 EQU   513                 0% IS MIN COM %                              
COMPERR5 EQU   514                 1ST CHAR IS + OR -                           
CBASERR1 EQU   522                 COMM BASIS REQUIRED                          
CBASERR2 EQU   521                 COMM BASIS GROSS OR NET                      
EDERR1   EQU   542                 DATE REQUIRES BILL BASIS                     
EDERR2   EQU   523                 DATE REQUIRED FOR BILL BASIS                 
GSTERR   EQU   524                 INVALID GST CODE                             
PSTERR1  EQU   528                 INVALID PST CODE                             
EDTERR1  EQU   530                 INPUT CANNOT BE NUMERIC                      
EDTERR2  EQU   529                 INPUT MUST BE NUMERIC                        
OPTERR1  EQU   531                 OPTION MUST BE NTP                           
OPTERR2  EQU   532                 NTP MUST BE 0-2                              
VKERR1   EQU   533                 MUST BE 3 LONG                               
VKERR2   EQU   534                 MUST BE VALID HEX                            
VKERR3   EQU   535                 ZZZ INVALID PRD CODE                         
VKERR4   EQU   536                 CLIENT MUST BE 'CC'                          
VKERR5   EQU   537                 1ST CHAR MUST BE ALPHA                       
VKERR6   EQU   538                 MUST BE 2 OR 3 CHARS LONG                    
VKERR7   EQU   539                 2ND AND 3RD CHARS ALPHANUMERIC               
VKERR8   EQU   540                 ALL IS INVALID PRD CODE                      
VKERR9   EQU   541                 NO IS INVALID PRD CODE                       
CHGERR   EQU   546                 CANNOT CHANGE FIELD                          
NTPERR3  EQU   547                 NTP CAN ONLY BE SET ONCE                     
DELERR1  EQU   548                 PRODUCT EXISTS FOR GROUP IN ID               
DELERR2  EQU   549                 MASTER TRAFFIC CLT ALREADY EXISTS            
DELERR3  EQU   550                 BILL RECS SHOULD SUM TO ZERO                 
DELERR4  EQU   551                 ORDERED OR PAID $ ON EST                     
DELERR5  EQU   552                 BILL ON FILE- CANNOT DEL PRD                 
DELERR6  EQU   553                 NO GOALS-- GO CHECK FOR BUYS                 
DELERR7  EQU   554                 HAS BUYS, CANNOT DELETE                      
DELERR8  EQU   555                 CANNOT DELETE POL PRODUCT                    
DELERR9  EQU   560                 'DELETE' MUST BE IN PROD NAME                
PFERR    EQU   559                 INVALID PFKEY                                
BRPOLER1 EQU   561                 BRAND & POL EST DATES MUST AGREE             
EDOLERR  EQU   562                 ORDERED OR PAID $$ ON AN EST                 
ESTERR1  EQU   563                 ESTIMATE CODE MUST BE NUMERIC                
ESTERR2  EQU   564                 ESTIMATE CODE BETWEEN 1 - 255                
ESTERR3  EQU   565                 NO POL EST OPEN FOR BRAND POL CLT            
ESTERR4  EQU   566                 POL MSTR OR SUB EST OPEN - NO ADD            
ESTERR5  EQU   567                 CANNOT CHANGE DATES                          
ESTERR6  EQU   568                 CANNOT SHORTEN DURATION                      
ESTERR7  EQU   569                 END DATE BEFORE START DATE                   
ESTERR8  EQU   570                 SYS=NETPACK, MEDIA MUST = N                  
ESTERR9  EQU   571                 CAN'T HAVE PW ON EST > 14 WKS                
ESTERR10 EQU   572                 NO OUT-OF-WEEK ROTATOR                       
TV1DEM   EQU   573                 TV MUST HAVE AT LEAST 1 DEMO                 
NET1DEM  EQU   721                 NETPAK MUST HAVE AT LEAST 1 DEMO             
TOTHERR  EQU   576                 TOTAL HMS MUST BE INPUT FOR NETWORK          
DEMERR   EQU   584                 INVALID OR TOO MANY DEMOS                    
DEMERR2  EQU   585                 DEMO NOT IN POL DEMOS                        
WGHTERR  EQU   586                 INVALID OR TOO MANY WEIGHTS                  
WGHTERR2 EQU   587                 NO WEIGHTS FOR RATINGS                       
WGHTERR3 EQU   588                 WEIGHT INVALID OR MISSING                    
WGHTERR4 EQU   589                 NO WEIGHTED DEMO                             
WGHTERR5 EQU   590                 NO WEIGHTS                                   
FNDERR   EQU   591                 RECORD NOT FOUND                             
FLT1MSS  EQU   592                 FILTER 1 MISSING                             
FLT2MSS  EQU   593                 FILTER 2 MISSING                             
FLT3MSS  EQU   594                 FILTER 3 MISSING                             
DLYERR   EQU   595                 DAILY EST MAX 53 DAYS                        
REQERR   EQU   596                 ONLY VALID FOR POL EST                       
ESTERR   EQU   597                 NOT IN EST PERIOD                            
PRIDERR  EQU   654                 PRIMARY DEMOS MUST MATCH                     
ENDOLAP  EQU   655                 END DATE OVERLAPS                            
INVES    EQU   656                 ESTIMATE INVALID                             
ETYPERR  EQU   657                 ETYPE CAN'T BE DELETED                       
STRINJAN EQU   658                 START MONTH MUST BE JAN                      
ENDINDEC EQU   659                 END MONTH MUST BE DEC                        
DUPERR   EQU   660                 DUPLICATE FOUND                              
DSPERR   EQU   661                 INVALID DATE SPREAD                          
DAILYERR EQU   662                 DAILY EST MAX 53 DAYS                        
POLRQERR EQU   663                 ONLY VALID FOR POL EST                       
NOPWPCT  EQU   664                 PW % IS REQUIRED                             
NOTPOLPW EQU   665                 BRAND PW MUST = POL PW %                     
DELPWPOL EQU   666                 MUST REMOVE PW% FROM POL                     
CASHINV  EQU   667                 CASH OPTION INVALID FOR NON TRD PRD          
CASHREQ  EQU   668                 CASH OPTION REQ'D FOR TRADE PRDS             
NOCSHEST EQU   669                 NO ESTIMATE FOR CASH PRD                     
CASHSET  EQU   670                 CASH ESTIMATE ASSIGNED DIF TRD PRD           
CASHCHG  EQU   671                 CAN NOT CHANGE CASH PRODUCT                  
CASHTRD  EQU   672                 CASH PRD CANNOT BE TRADE PRD                 
SPTLNERR EQU   673                 SPOT LENGTH NOT VALID                        
INVPW    EQU   674                 CAN'T HAVE PW ON EST >14WKS                  
RATERR1  EQU   675                 RATING BOOK REQUIRED                         
HUTERR1  EQU   676                 HUT ADJ REQUIRED                             
TOOBIG   EQU   677                 TOO LONG                                     
STATERR1 EQU   679                 LOCK STATUS NOT VALID FOR ADDS               
STATERR2 EQU   680                 STATUS INVALID FOR PREV HELD EST             
STATERR3 EQU   681                 HOLD STATUS NOT VALID FOR ADDS               
STATERR4 EQU   682                 UNLOCK NOT VALID FOR ADDS                    
STATERR5 EQU   683                 CAN'T UNLOCK HELD ESTIMATE                   
STATERR6 EQU   684                 EST NOT PREV LOCKED                          
STATERR7 EQU   685                 REL NOT VALID STATUS FOR ADDS                
STATERR8 EQU   686                 DBT NOT VALID STATUS FOR ADDS                
STATERR9 EQU   687                 MUST BE FOLLOWED BY YEAR (2 DIGITS)          
STATERRA EQU   688                 CAN'T USE DATE FOR HELD ESTIMATE             
STATERRB EQU   689                 CAN'T USE DATE FOR LOCKED ESTIMATE           
STATERRC EQU   690                 STATUS REQ'D FOR NETPACK ADDS                
DESERR1  EQU   691                 DESCRIPTION REQ'D                            
DATERR1  EQU   692                 START DATE REQUIRED                          
DATERR2  EQU   693                 END DATE REQUIRED                            
OWRERR1  EQU   694                 O-W-R MUST START W/ N OR Y                   
OWRERR2  EQU   695                 O-W-R MUST START W/ N                        
DEMERR3  EQU   696                 MENU OPTION MUST BE 1-4 C'S LONG             
DEMERR4  EQU   697                 MENU OPTION INVALID                          
MENERR1  EQU   698                 DEPT MENU REQUIRED                           
MENERR2  EQU   699                 DEPT MENU MUST BE 1 CHAR LONG                
BRPOLER2 EQU   701                 BRAND & POL OOWR MUST AGREE                  
BRPOLER3 EQU   702                 BRAND & POL RATING BOOK MUST AGREE           
BRPOLER4 EQU   703                 BRAND & POL DEPT MENY MUST AGREE             
BRPOLER5 EQU   704                 BRAND & POL HUT ADJ'S MUST AGREE             
BRPOLER6 EQU   705                 BRAND & POL CONTROL'S MUST AGREE             
BRPOLER7 EQU   706                 BRAND & POL RET SCHEME MUST AGREE            
BRPOLER8 EQU   707                 BRAND & POL FILTERS MUST AGREE               
BRPOLER9 EQU   708                 BRAND & POL WEIGHTS MUST AGREE               
CONTER1  EQU   709                 BILLING RECORDS EXIST                        
CONTER2  EQU   710                 BILLING RECORDS EXIST FOR BRAND EST          
CPPERR1  EQU   711                 FOR POL, CPP MUST BE NUMERIC                 
TYPERR1  EQU   712                 TYPE CANNOT BE 'CUT'                         
TYPERR2  EQU   713                 TYPE ONLY VALID FOR POL EST                  
TYPERR3  EQU   714                 TYPE MUST BE 2 CHARACTERS LONG               
TYPERR4  EQU   715                 TYPE MUST MATCH CPP EST TYPE                 
RANERR1  EQU   716                 FIRST HALF LESS THAN SECOND                  
RANERR2  EQU   717                 CODE MUST FIT IN RANGE                       
RATER1   EQU   718                 CLIENT REC DOES NOT ALLOW SPEC RATES         
NODEMODE EQU   722                 CAN'T DELETE DEMO FROM POL                   
DEMINVL2 EQU   724                 DEMO NOT IN POL ESTIMATE                     
USDMER   EQU   725                 BRAND USER NAMES MUST MATCH POL              
USWDMER  EQU   726                 BRAND WEIGHT. DEMO MUST MATCH POL            
CHAWT1   EQU   727                 WEIGHT DOES NOT MATCH POL EST                
CHAWT2   EQU   728                 CANNOT CHANGE WEIGHT ON POL EST              
CASH1    EQU   729                 INVALID CASH PRODUCT                         
CASH2    EQU   730                 CASH PRODUCT DOES NOT EXIST FOR CLT          
CPPERR2  EQU   731                 CPP EST MUST BE NUMERIC FOR POL EST          
REQERR1  EQU   732                 REQ OPTION MUST BE Y OR N                    
CGTERR   EQU   733                 CGT < = $99.99                               
ODEERR   EQU   734                 DEMO OPTION MUST BE Y OR N                   
COS2ERR  EQU   735                 COS2 MUST BE 0<X<9.99                        
MONDERR  EQU   736                 START DATE CANNOT BE MONDAY                  
ONLY1DIY EQU   782                 CAN ONLY HAVE 1 TRADE PRD PER EST            
DEMOEXC  EQU   797                 EXCEEDS MAXIMUM DEMOS.                       
MULTCOS2 EQU   903                 MORE THEN 1 COS2 ENTRY                       
TYPERR   EQU   985                 TYPE OPT MUST BE STW,REG,BAR(TER)            
BRPOLERA EQU   990                 BRAND & POL TYPE=OPTS MUST AGREE             
TYPEERR1 EQU   1145                TYPE=STW/BAR NOT ALLOWED                     
WTSTERRS EQU   1211                MUST ENTER T1 AND T2 DEMOS                   
*                                                                               
         LTORG                                                                  
*                                                                               
* TABLES                                                                        
***********************************************************************         
*        CONSTANTS                                                    *         
***********************************************************************         
EOTBLQ   DC    C'A'                                                             
DMAX     EQU   20                                                               
*                                                                               
***********************************************************************         
*        VALID NUMERICS TABLE                                         *         
***********************************************************************         
VALDNTBL DC    C' 0123456789-/'                                                 
*                                                                               
*                                                                               
***********************************************************************         
*        TARGET NUMBER LIST                                           *         
***********************************************************************         
ONETWO   DC    X'0102FF'                                                        
*                                                                               
***********************************************************************         
*        AGENCY TABLE                                                 *         
***********************************************************************         
AGYTAB   DC   C'GY'                                                             
         DC   C'DR'                                                             
         DC   C'GN'                                                             
         DC   C'CE'                                                             
         DC   C'FM'                                                             
         DC   C'RE'                                                             
         DC   X'FF'                                                             
***********************************************************************         
*        ECTAGY                                                       *         
***********************************************************************         
ECTAGY   DC    C'WI'                                                            
         DC    C'WR'                                                            
         DC    C'WT'                                                            
         DC    C'WJ'               WITEST                                       
         DC    C'SJ'                                                            
         DC    C'SX'                                                            
         DC    C'FC'                                                            
         DC    C'BS'                                                            
         DC    C'TH'                                                            
         DC    X'FF'                                                            
***********************************************************************         
*        TYPTAB                                                       *         
***********************************************************************         
TYPTAB   DC    C'M$',X'03'                                                      
         DC    C'M%',X'04'                                                      
         DC    C'Q$',X'05'                                                      
         DC    X'FF'                                                            
*                                                                               
***********************************************************************         
*        LINE ID TABLE                                                *         
***********************************************************************         
LINIDTAB DC    C'DD13C2D1'                                                      
         DC    C'DDL1136T'          DDS-LA                                      
         DC    C'DDL1137T'          DDS-LA                                      
         DC    C'DDL1138T'          DDS-LA                                      
         DC    C'DX06200T'         (WAS DDNY720T)                               
         DC    C'DDNY700T'                                                      
         DC    C'DDNYD03T'                                                      
*NOP*    DC    C'DX03901T'                                                      
         DC    C'DDNY916T'                                                      
*NOP*    DC    C'DDNY720T'                                                      
*NOP*    DC    C'DDNYF11T'          DDS                                         
         DC    C'HDTO847T'          HDTO (WAS HDTO823T)                         
         DC    C'HDTO829T'          HDTO (WAS HDTO830T)                         
         DC    C'XDDSC84A'                                                      
*NOP*    DC    C'DDNYD26T'                                                      
         DC    X'FF'                                                            
*                                                                               
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
*                                                                               
***********************************************************************         
*        SETUP                                                        *         
***********************************************************************         
SETUP    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LHI   R0,(BHSTAGH-BHSSELH)/(BHSL2H-BHSSELH)                            
         STC   R0,NLISTS                                                        
*                                                                               
         OI    GENSTAT1,USKYMRG+NOSETEFH                                        
         OI    CONSERVH+1,X'01'    MODIFY SERVICE REQUEST                       
         OI    CONSERVH+6,X'80'    TRANSMIT TO GET CONTROL                      
         OI    GENSTAT4,NODELLST   NO DELETION ALLOWED                          
         OI    GENSTAT2,DISTHSPG                                                
*                                                                               
SETUP01  MVI   USEIO,C'N'                                                       
*                                                                               
         L     RF,ACOMFACS                                                      
         USING COMFACSD,RF                                                      
         GOTO1 GETFACT,DMCB,(2,0)                                               
         L     R1,0(R1)              SET MEDIA IN USE                           
         USING FACTSD,R1                                                        
         MVC   OVSYS,FAOVSYS         2=SPOT,3=NET                               
         DROP  R1,RF                                                            
*                                                                               
         BRAS  RE,SSV                                                           
*                                                                               
SETUPX   XIT1                                                                   
         LTORG                                                                  
*                                                                               
*                                                                               
********************************************************************            
*                     SET SYSTEM VALUES                            *            
********************************************************************            
SSV      NTR1  BASE=*,LABEL=*                                                   
         MVC   LKEY,=H'32'             DETAILS OF DIRECTORY AND KEY             
         MVC   LSTATUS,=H'4'                                                    
         MVC   DATADISP,=H'42'                                                  
         MVC   SYSFIL,=C'XSPFIL  '                                              
         MVC   SYSDIR,=C'XSPDIR  '                                              
         MVI   USEIO,0                                                          
         XC    FILENAME,FILENAME                                                
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
********************************************************************            
*                   RESET SYSTEM VALUES                            *            
********************************************************************            
RSV      NTR1  BASE=*,LABEL=*                                                   
         MVC   LKEY,=H'13'         DETAILS OF DIRECTORY AND KEY                 
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'24'     USUALLY SPOTFILE                             
         MVC   SYSFIL,=C'SPTFIL  '                                              
         MVC   SYSDIR,=C'SPTDIR  '                                              
         MVI   USEIO,0                                                          
         XC    FILENAME,FILENAME                                                
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
*                                                                               
********************************************************************            
*                   RESET SYSTEM VALUES                            *            
********************************************************************            
SETUNIT  NTR1  BASE=*,LABEL=*                                                   
         MVC   LKEY,=H'20'                                                      
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'27'                                                  
         MVC   SYSFIL,=C'UNTFIL  '                                              
         MVC   SYSDIR,=C'UNTDIR  '                                              
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
*                       PUT RECORD                                    *         
***********************************************************************         
*                                                                               
PTREC    NTR1  BASE=*,LABEL=*                                                   
         MVI   USEIO,C'Y'                                                       
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'SPTFIL',KEY+14,AIO                     
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   USEIO,C'N'                                                       
         J     EQXIT                                                            
*                                                                               
***********************************************************************         
*                       ADD RECORD                                    *         
***********************************************************************         
*                                                                               
ADREC    NTR1  BASE=*,LABEL=*                                                   
         MVI   USEIO,C'Y'                                                       
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'SPTFIL',KEY+14,AIO                     
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   USEIO,C'N'                                                       
         J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
***********************************************************************         
*                       CLEAR SCREEN                                  *         
***********************************************************************         
CLR     NTR1   BASE=*,LABEL=*                                                   
        LA     R2,BHNEDTH                                                       
*                                                                               
CLR10   CLI    0(R2),0             END OF SCREEN ?                              
        BE     CLRX                DONE                                         
*                                                                               
        TM     1(R2),X'20'         DO NOT CLEAR PROTECTED FIELDS                
        BO     CLR50                                                            
        ZIC    RE,0(R2)                                                         
        SH     RE,=H'8'                                                         
        TM     1(R2),X'02'         EXTENDED HEADER ?                            
        BZ     *+8                 YES, SUBTRACT EXTENSION LEN                  
        SH     RE,=H'8'                                                         
        BCTR   RE,0                                                             
        EX     RE,*+8                                                           
        B      *+10                                                             
        XC     8(0,R2),8(R2)                                                    
        OI     6(R2),X'80'                                                      
*                                                                               
CLR50   ZIC    RE,0(R2)                                                         
        AR     R2,RE                                                            
        B      CLR10                                                            
*                                                                               
CLRX    J      EQXIT                                                            
*                                                                               
*                                                                               
*******************************************************************             
*        CHECK THE FILTERS                                        *             
*******************************************************************             
FILTERS  NTR1  BASE=*,LABEL=*                                                   
         LA    R4,KEY                                                           
         USING BHLNRECD,R4                  FILTER BY:                          
*                                                                               
         OC    SVBCLT,SVBCLT                CLIENT?                             
         BZ    FLT10                                                            
         CLC   SVBCLT,KEY+BHLNKCLI-BHLNKEY                                      
         BNE   FLTNQX                                                           
*                                                                               
FLT10    OC    SVPRD,SVPRD                  PRODUCT?                            
         BZ    FLT20                                                            
         CLC   BHLNKPRD,KEY+BHLNKPRD-BHLNKEY                                    
         BNE   FLTNQX                                                           
*                                                                               
FLT20    OC    SVEST,SVEST                  ESTIMATE?                           
         BZ    FLT30                                                            
         CLC   SVEST,KEY+BHLNKEST-BHLNKEY                                       
         BNE   FLTNQX                                                           
*                                                                               
FLT30    OC    SVNET,SVNET                  NETWORK                             
         BZ    FLT40                                                            
         CLC   SVNET,KEY+BHLNKNET-BHLNKEY                                       
         BNE   FLTNQX                                                           
*                                                                               
FLT40    OC    SVMOS,SVMOS                                                      
         BZ    FLT50                                                            
         CLC   SVMOS,KEY+BHLNKMOS-BHLNKEY                                       
         BNE   FLTNQX                                                           
*                                                                               
FLT50    OC    SVPKG,SVPKG                                                      
         BZ    FLTQX                                                            
         CLC   SVPKG,KEY+BHLNKPKG-BHLNKEY                                       
*                                                                               
FLTQX    J     EQXIT                                                            
FLTNQX   J     NEQXIT                                                           
         DROP  R4                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
***********************************************************************         
* R2 EXPECTED TO ADDRESS PACKAGE FIELD HEADER                                   
* ON RETURN - SVPKG WILL HAVE BINARY PACKAGE VALUE                              
* UNEQUAL CONDITION CODE SET IF PKG IS INVALID                                  
***********************************************************************         
VPKG     NTR1  BASE=*,LABEL=*                                                   
         XC    SVPKG,SVPKG                                                      
*                                                                               
         CLC   =C'ALL',8(R2)                                                    
         BNE   VPKG10                                                           
         CLI   5(R2),3                                                          
         BE    VPKGQX                                                           
         B     VPKGNQX                                                          
*                                                                               
VPKG10   DS    0H                                                               
         TM    4(R2),X'08'                                                      
         BZ    VPKGNQX                                                          
*                                                                               
         ZIC   R1,BHNPKGH+5                                                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,BHNPKG(0)                                                    
         CVB   R1,DUB                                                           
         CHI   R1,1                                                             
         BL    VPKGNQX                                                          
         CHI   R1,255                                                           
         BH    VPKGNQX                                                          
         STC   R1,SVPKG                                                         
*                                                                               
         CLI   ACTNUM,ACTLIST                                                   
         BE    VPKGQX                                                           
*                                                                               
         MVC   SAVEKEY,KEY                                                      
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING NPKEY,R4                                                         
         MVI   KEY,X'02'                                                        
         MVC   NPKAM,SVAM                                                       
         MVC   NPKCLT,SVBCLT                                                    
         MVC   NPKNET,SVNET                                                     
         MVC   NPKEST,SVEST                                                     
         MVC   NPKPACK,SVPKG                                                    
*                                                                               
         BRAS  RE,SETUNIT                                                       
         GOTO1 HIGH                                                             
         BRAS  RE,SSV                                                           
         CLC   KEY(L'NPKEY),KEYSAVE                                             
         BNE   VPKGNQX                                                          
*                                                                               
VPKGQX   J     EQXIT                                                            
*                                                                               
VPKGNQX  XC    SVPKG,SVPKG                                                      
         J     NEQXIT                                                           
         LTORG                                                                  
*                                                                               
*                                                                               
***********************************************************************         
* R2 EXPECTED TO ADDRESS MOS FIELD HEADER                                       
* ON RETURN - SVMOS WILL HAVE BINARY MOS VALUE                                  
* UNEQUAL CONDITION CODE SET IF MOS IS INVALID                                  
***********************************************************************         
VMOS     NTR1  BASE=*,LABEL=*                                                   
         XC    SVMOS,SVMOS                                                      
*                                                                               
         CLC   =C'ALL',8(R2)                                                    
         BNE   VMOS10                                                           
         CLI   5(R2),3                                                          
         BE    VMOSQX                                                           
         B     VMOSNQX                                                          
*                                                                               
VMOS10   DS    0H                                                               
         GOTO1 DATVAL,DMCB,(2,8(R2)),CHARYMD                                    
         OC    DMCB,DMCB           IS DATE VALID?                               
         BZ    VMOSNQX                                                          
*                                                                               
         GOTO1 DATCON,DMCB,(0,CHARYMD),(3,BINYMD)                               
         MVC   SVMOS,BINYMD                                                     
*                                                                               
VMOSQX   J     EQXIT                                                            
*                                                                               
VMOSNQX  J     NEQXIT                                                           
         LTORG                                                                  
*                                                                               
*                                                                               
***********************************************************************         
* R1 EXPECTED TO ADDRESS BINARY CLIENT                                          
* ON EXIT - WORK WILL HAVE THE 3-CHAR CLIENT CODE, WORK+3 - CLT NAME            
***********************************************************************         
DISBCLT  NTR1  BASE=*,LABEL=*                                                   
         LR    R3,R1                                                            
         BRAS  RE,RSV                                                           
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),SVAM                                                    
         MVC   KEY+2(2),0(R3)                                                   
         MVC   AIO,AIO2                                                         
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         L     R1,AIO                                                           
         MVC   BYTE,CPROF+6-CLTHDR(R1)                                          
         MVC   WORK+3(L'CNAME),CNAME-CLTHDR(R1)                                 
         GOTO1 CLUNPK,DMCB,(BYTE,0(R3)),WORK                                    
         J     EQXIT                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
***********************************************************************         
* R1 EXPECTED TO ADDRESS BHLNKEY                                                
* ON EXIT - WORK WILL HAVE THE 3-CHAR PRD CODE, WORK+3 - PRD NAME               
***********************************************************************         
DISPRD   NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,RSV                                                           
         LR    R3,R1                                                            
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),SAVEKEY+2  A/M,CLT                                      
         MVC   KEY+4(3),0(R3)      PRODUCT                                      
         MVC   AIO,AIO2                                                         
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         L     R1,AIO                                                           
         MVC   WORK(3),PKEYPRD-PKEY(R1)                                         
         MVC   WORK+3(L'PNAME),PNAME-PRDHDR(R1)                                 
         J     EQXIT                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
***********************************************************************         
* R1 EXPECTED TO ADDRESS BHLNKEY                                                
* ON EXIT - WORK WILL HAVE THE 3-CHAR EST CODE, WORK+3 - EST NAME               
***********************************************************************         
DISEST   NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,RSV                                                           
         LR    R3,R1                                                            
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(6),SAVEKEY+2                                               
         MVC   KEY+7(1),0(R3)                                                   
         MVC   AIO,AIO2                                                         
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         L     R1,AIO                                                           
         ZIC   RF,EKEYEST-EKEY(R1)                                              
         MVC   SVESTART,ESTART-EKEY(R1)                                         
         MVC   SVEEND,EEND-EKEY(R1)                                             
         EDIT  (RF),(3,WORK),ALIGN=LEFT                                         
         MVC   WORK+3(L'EDESC),EDESC-ESTHDR(R1)                                 
         J     EQXIT                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
***********************************************************************         
*        DSECTS                                                       *         
***********************************************************************         
PRODUCT DSECT                                                                   
       ++INCLUDE SPGENPRD          PRODUCT RECORD                               
*                                                                               
CLIENT  DSECT                                                                   
       ++INCLUDE SPGENCLT          CLIENT  RECORD                               
*                                                                               
       ++INCLUDE SPGENAGY          AGENCY  RECORD                               
*                                                                               
       ++INCLUDE SPGENEST          ESTIMATE RECORDS                             
*                                                                               
       ++INCLUDE SPGENBILL         BILLING RECORDS                              
*                                                                               
       ++INCLUDE NEGENBHOLD        BILLING HOLD RECORDS                         
*                                                                               
       ++INCLUDE SPGENMKT          MARKET RECORD                                
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
*                                                                               
       ++INCLUDE DDSPLWORKD                                                     
*                                                                               
         PRINT ON                                                               
*                                                                               
       ++INCLUDE NESFMFFD                                                       
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE NESFM98D                                                       
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE NESFM99D                                                       
*                                                                               
       ++INCLUDE DDGENTWA                                                       
*                                                                               
       ++INCLUDE DDPSTBLK          PROVINCIAL TAX VALIDATION                    
*                                                                               
       ++INCLUDE DDCOREQUS                                                      
*                                                                               
       ++INCLUDE DDCOMFACS                                                      
*                                                                               
       ++INCLUDE DEDBLOCK                                                       
*                                                                               
       ++INCLUDE FAFACTS                                                        
*                                                                               
       ++INCLUDE DDPARSNIPD                                                     
*                                                                               
       ++INCLUDE DDFLDIND                                                       
*                                                                               
       ++INCLUDE SPGENAPY          AUTOPAY RECORD DSECT                         
*                                                                               
       ++INCLUDE DDSCANBLKD        FOR SCANNER                                  
*                                                                               
       ++INCLUDE DDOFFICED                                                      
*                                                                               
       ++INCLUDE FAGETTXTD         ERROR MSGS                                   
*                                                                               
       ++INCLUDE NEGENPACK         NETWORK PACKAGE RECORDS                      
*                                                                               
SCAND    DSECT                       DSECT TO COVER SCANNER LINES               
FLD1LEN  DS    CL1                                                              
FLD2LEN  DS    CL1                                                              
FLD1VAL  DS    CL1                                                              
FLD2VAL  DS    CL1                                                              
FLD1B    DS    CL4                                                              
FLD2B    DS    CL4                                                              
FLD1     DS    CL10                                                             
FLD2     DS    CL10                                                             
*                                                                               
BSCAND   DSECT                     SCANNER LIKE OUTPUT                          
BFLD1LEN DS    CL1                                                              
BFLD2LEN DS    CL1                                                              
BFLD1VAL DS    CL1                                                              
BFLD2VAL DS    CL1                                                              
BFLD1B   DS    CL4                                                              
BFLD2B   DS    CL4                                                              
BFLD1    DS    CL11                EXCEPT FLD 1 = LEN 11                        
BFLD2    DS    CL10                                                             
BSCANLNQ EQU   *-BSCAND                                                         
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LSTCLT   DS    CL3                                                              
         DS    CL1                                                              
LSTPRD   DS    CL3                                                              
         DS    CL1                                                              
LSTEST   DS    CL3                                                              
         DS    CL1                                                              
LSTNET   DS    CL4                                                              
         DS    CL1                                                              
LSTMOS   DS    CL6                                                              
         DS    CL1                                                              
LSTPKG   DS    CL3                                                              
         DS    CL1                                                              
LSTEDATE DS    CL8                                                              
*                                                                               
*                                                                               
       ++INCLUDE NESFMWORKD                                                     
*                                                                               
***********************************************************************         
*        SAVED STORAGE DSECT                                          *         
***********************************************************************         
*                                                                               
SYSD     DSECT                                                                  
         ORG   SYSSPARE                                                         
RELO     DS    A                   RELOCATION FACTOR                            
BHKEY    DS    CL32                                                             
SAVEKEY  DS    CL32                SAVE AREA TO RESTORE READ SEQUENCE           
ERRNUM   DS    XL2                                                              
FAKEFLD  DS    XL28                                                             
OVSYS    DS    X                                                                
PREVFLAG DS    C                                                                
PREVKEY  DS    XL48                                                             
*                                                                               
SVFLDS   DS    0X                                                               
SVAM     DS    X                                                                
SVBCLT   DS    XL2                                                              
SVPRD    DS    CL3                                                              
SVEST    DS    X                                                                
SVESTART DS    CL6                 START DATE (YYMMDD)                          
SVEEND   DS    CL6                 END DATE (YYMMDD)                            
SVNET    DS    CL4                                                              
SVMOS    DS    XL2                                                              
SVPKG    DS    X                                                                
SVFLDSLQ EQU   *-SVFLDS                                                         
*                                                                               
* --- USED FOR CONVERTING THE DATES (EX. JAN/96 -TO- JAN01/96)                  
*                                                                               
NTMPDT   DS    0CL8                                                             
NTMPMON  DS    CL3                                                              
NTMPDAY  DS    CL2                                                              
NTMPYR   DS    CL3                                                              
CHARYMD  DS    CL6                                                              
COMPYMD  DS    CL2                                                              
BINYMD   DS    XL3                                                              
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'084NESFM55   10/22/09'                                      
         END                                                                    
