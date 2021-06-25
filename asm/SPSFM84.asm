*          DATA SET SPSFM84    AT LEVEL 003 AS OF 08/25/04                      
*PHASE T21784A                                                                  
         TITLE 'SPSFM84 - SUPERDESK UDEF PROGRAM'                               
******************************************************************              
* REGISTERS:  R0 -- WORK                                                        
*             R1 -- WORK                                                        
*             R2 -- SCREEN FIELD HEADER                                         
*             R3 -- WORK                                                        
*             R4 -- WORK                                                        
*             R5 -- WORK                                                        
*             R6 -- GETEL REGISTER                                              
*             R7 -- SECOND BASE                                                 
*             R8 -- SPOOL                                                       
*             R9 -- SYSD                                                        
*             RA -- TWA                                                         
*             RB -- FIRST BASE                                                  
*             RC -- GEND                                                        
*             RD -- SYSTEM                                                      
*             RE -- SYSTEM                                                      
*             RF -- SYSTEM                                                      
******************************************************************              
         EJECT                                                                  
******************************************************************              
*                   MAIN PROGRAM                                 *              
******************************************************************              
T21784   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1784**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
*                                                                               
         OI    CONSERVH+1,X'01'    MODIFY SERVICE REQUEST                       
         OI    CONSERVH+6,X'80'    TRANSMIT TO GET CONTROL                      
*                                                                               
         OI    GENSTAT4,NODELLST   DISALLOW DELETES FROM LST SCREEN             
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
**       CLI   MODE,PRINTREP       PRINT RECORDS                                
**       BE    PR                                                               
         CLI   MODE,RECDEL         ID REC THAT GENCON ABOUT TO DELETE           
         BE    RDEL                                                             
         B     EXIT                                                             
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
******************************************************************              
*                         VALIDATE KEY                           *              
******************************************************************              
*                                                                               
VK       DS    0H                                                               
         CLI   ACTNUM,ACTLIST                                                   
         BE    VK05                                                             
         XC    SUDCLNM,SUDCLNM                                                  
         OI    SUDCLNMH+6,X'80'                                                 
*                                                                               
* MEDIA FIELD                                                                   
*                                                                               
VK05     LA    R2,SUDMEDH          MEDIA                                        
         MVI   USEIONUM,2          READ INTO AIO2                               
         GOTO1 VALIMED             VALIDATE MEDIA CODE                          
*                                                                               
* CLIENT FIELD                                                                  
*                                                                               
         XC    BCLT,BCLT                                                        
         LA    R2,SUDCLTH          CLIENT FIELD                                 
         CLI   5(R2),0                                                          
         BE    VK20                                                             
         CLC   8(3,R2),=C'ALL'     CLIENT ALL                                   
         BE    VK20                                                             
         MVI   USEIONUM,2          READ INTO AIO2                               
         GOTO1 VALICLT             VALIDATE                                     
         CLI   ACTNUM,ACTLIST                                                   
         BE    *+10                                                             
         MVC   SUDCLNM,CLTNM       AND TRANSMIT THE NAME                        
*                                                                               
         USING SDURECD,R6                                                       
VK20     LA    R6,KEY                                                           
         XC    KEY,KEY                                                          
         MVC   SDUKTYP,=X'0D08'                                                 
         MVC   SDUKAM,BAGYMD                                                    
         MVC   SDUKCLT,BCLT                                                     
         MVC   SAVEKEY,KEY                                                      
*                                                                               
         MVC   AIO,AIO1            JUST IN CASE                                 
         MVI   USEIONUM,1                                                       
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
******************************************************************              
*                         VALIDATE RECORD                        *              
******************************************************************              
*                                                                               
         USING SDURECD,R6                                                       
VR       DS    0H                                                               
         L     R6,AIO                                                           
*                                                                               
         MVI   ELCODE,X'20'        REMOVE ALL ELEMS FIRST                       
         GOTO1 REMELEM                                                          
*                                                                               
         ZAP   TOTLEN,=P'0'                                                     
         LA    R4,1                WHICH FIELD                                  
         LA    R2,SUDF1H           START WITH 1                                 
*                                                                               
VR10     CLI   5(R2),0             IS DESC FIELD EMPTY ?                        
         BNE   VR20                                                             
         BAS   RE,CHKROW           YES, SEE IF WHOLE ROW IS EMPTY               
         B     VRNEXT              IF RETURNED, POINTING ROW                    
*                                                                               
         USING SDUDELD,R6          REBUILD NEW                                  
VR20     XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         MVI   SDUDEL,X'20'                                                     
         MVI   SDUDLN,SDUDLNQ                                                   
*                                                                               
         STC   R4,SDUDNUM          FIELD NUMBER                                 
         MVC   SDUDDESC,8(R2)      DESCRIPTION                                  
*                                                                               
         BAS   RE,BMP2XMIT         BUMP 2 SCREEN FIELDS                         
*                                                                               
         CLI   5(R2),0             REQ'D FIELD                                  
         BE    ERRMISS                                                          
         CLI   8(R2),C'N'                                                       
         BE    VR30                                                             
         CLI   8(R2),C'Y'                                                       
         BNE   ERRINV                                                           
         OI    SDUDFLAG,SDUDFREQ   FIELD REQ'D                                  
*                                                                               
VR30     BAS   RE,BMP2XMIT         BUMP 2 SCREEN FIELDS                         
*                                                                               
         MVI   SDUDEDIT,SDUDEBLK   EDIT RULE= BLANK DEFAULT                     
         CLI   5(R2),0             MISSING IS OK = NO RULE                      
         BE    VR40                                                             
         CLI   8(R2),C' '                                                       
         BNH   VR40                                                             
         MVI   SDUDEDIT,SDUDEYN    Y/N EDIT                                     
         CLI   8(R2),C'Y'                                                       
         BE    VR40                                                             
         MVI   SDUDEDIT,SDUDEDT    DATE EDIT                                    
         CLI   8(R2),C'D'                                                       
         BE    VR40                                                             
         MVI   SDUDEDIT,SDUDECH    CHAR EDIT                                    
         CLI   8(R2),C'C'                                                       
         BE    VR40                                                             
         MVI   SDUDEDIT,SDUDENN    NUMERIC EDIT                                 
         CLI   8(R2),C'N'                                                       
         BE    VR40                                                             
         B     ERRINV                                                           
*                                                                               
VR40     BAS   RE,BMP2XMIT         BUMP 2 SCREEN FIELDS                         
*                                                                               
         OI    6(R2),X'80'                                                      
         CLI   SDUDEDIT,SDUDEYN    Y/N EDIT                                     
         BNE   VR42                                                             
         MVI   8(R2),C'1'          FORCE LENGTH TO 1                            
         MVI   5(R2),1                                                          
         B     VR46                                                             
VR42     CLI   SDUDEDIT,SDUDEDT    DATE EDIT                                    
         BNE   VR44                                                             
         MVI   8(R2),C'2'          FORCE LENGTH TO 2                            
         MVI   5(R2),1                                                          
         B     VR46                                                             
VR44     TM    4(R2),X'08'         LENGTH - NUMERIC?                            
         BNO   ERRINV                                                           
VR46     ZIC   R1,5(R2)            CHK THE LENGTH LESS THAN 32                  
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R1,DUB                                                           
*                                                                               
         AP    TOTLEN,DUB          MAX INPUT LENGTHS TOTAL                      
         EDIT  (P8,TOTLEN),(2,SUDTOTL)                                          
         OI    SUDTOTLH+6,X'80'                                                 
*                                                                               
         CP    TOTLEN,=P'40'                                                    
         BH    ERROVLEN                                                         
         CHI   R1,1                                                             
         BL    ERRINV                                                           
         STC   R1,SDUDMLEN                                                      
*                                                                               
         GOTO1 ADDELEM                                                          
         DROP  R6                                                               
         BAS   RE,BMP2XMIT         BUMP 2 SCREEN FIELDS                         
         LA    R4,1(R4)            TO NEXT LINE                                 
         CHI   R4,5                                                             
         BNH   VR10                                                             
         B     VRX                                                              
*                                                                               
VRNEXT   BAS   RE,BMP2XMIT         BUMP 2 SCREEN FIELDS                         
         BAS   RE,BMP2XMIT         BUMP 2 SCREEN FIELDS                         
         BAS   RE,BMP2XMIT         BUMP 2 SCREEN FIELDS                         
         BAS   RE,BMP2XMIT         BUMP 2 SCREEN FIELDS                         
         LA    R4,1(R4)                                                         
         CHI   R4,5                                                             
         BNH   VR10                                                             
*                                                                               
VRX      CLI   ACTEQU,ACTADD                                                    
         BE    VRXX                                                             
**> ??   BAS   RE,COMPELEM         COMPARE NEW ELEMS WITH # OF OLD              
         B     VRXX                IF LESS IN REBUILT REC -- DIE!               
*                                                                               
***      OI    SUDMEDH+6,X'81'     SET MODIFIED (FORCE REVALIDATION)            
***      LA    R2,CONACTH                                                       
***      B     ERRINV                                                           
*                                                                               
VRXX     B     DR                  GOTO DISP REC LOGIC                          
         EJECT                                                                  
*                                                                               
********                                                                        
*  ROUTINE TO CHK IF WHOLE ROW IS EMPTY                                         
*  IF PARTIALLY EMPTY EXIT WITH ERROR                                           
********                                                                        
CHKROW   NTR1                                                                   
         ST    R2,SAVER2           POINTING TO DESC                             
         LA    R1,2                TEST REQ'D,EDIT,LENGTH                       
CR10     BAS   RE,BMP2XMIT         BUMP 2 SCREEN FIELDS                         
         CLI   5(R2),0             MISSING ?                                    
         BE    CR20                YES - GOOD                                   
         L     R2,SAVER2           POINT R2 BACK TO DESC                        
         B     ERRMISS                                                          
CR20     BCT   R1,CR10                                                          
         XIT1                                                                   
         EJECT                                                                  
******************************************************************              
* CHECK FOR ANY DATA INPUT ON ACTION ADD                                        
******************************************************************              
CKINPUT  NTR1                                                                   
*                                                                               
         LA    R2,SUDF1H           POINT TO 1ST DATA FIELD                      
CKI10    CLI   0(R2),0             END OF SCREEN ?                              
         BE    CKIX                                                             
         TM    1(R2),X'02'         EXTENSION ?                                  
         BZ    CKI20                                                            
         ZIC   RE,0(R2)            GET LEN                                      
         SHI   RE,8                                                             
         AR    RE,R2               RE POINTS TO XTENSION                        
         CLI   0(RE),30            DATA FIELD                                   
         BNE   CKI20               NO THEN SKIP                                 
         OC    8(10,R2),8(R2)      ANY INPUT                                    
         BNZ   EXIT                FOUND INPUT THEN RETURN                      
CKI20    ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         B     CKI10                                                            
*                                                                               
CKIX     LA    R2,SUDF1H           POINT TO 1ST DATA FIELD                      
         MVI   ERROR,MISSING       AND ASK FOR INPUT                            
         B     TRAPERR                                                          
*                                                                               
******************************************************************              
*                  DISPLAY RECORD                                *              
******************************************************************              
DR       DS    0H                                                               
         L     R6,AIO                                                           
         ZAP   TOTLEN,=P'0'                                                     
*                                                                               
         LA    R4,1                WHICH FIELD                                  
         LA    R2,SUDF1H           START WITH 1                                 
         MVI   ELCODE,X'20'                                                     
*                                                                               
         USING SDUDELD,R6                                                       
DR10NX   L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         B     *+8                                                              
DR10     BAS   RE,NEXTEL                                                        
         BNE   DR50                                                             
         ZIC   R1,SDUDNUM                                                       
         CR    R1,R4                                                            
         BNE   DR10                                                             
*                                                                               
         MVC   8(30,R2),SDUDDESC   DESCRIPTION                                  
         BAS   RE,BMP2XMIT         XMIT AND BUMP 2 SCREEN FLDS                  
*                                                                               
         MVI   8(R2),C'N'                                                       
         TM    SDUDFLAG,SDUDFREQ   FIELD REQ'D                                  
         BNO   *+8                                                              
         MVI   8(R2),C'Y'                                                       
         BAS   RE,BMP2XMIT         XMIT AND BUMP 2 SCREEN FLDS                  
*                                                                               
         MVC   8(1,R2),SDUDEDIT    EDIT RULE                                    
         BAS   RE,BMP2XMIT         XMIT AND BUMP 2 SCREEN FLDS                  
*                                                                               
         ZIC   R1,SDUDMLEN                                                      
         CVD   R1,DUB                                                           
         AP    TOTLEN,DUB                                                       
         EDIT  (1,SDUDMLEN),(2,8(R2)),ALIGN=LEFT                                
         BAS   RE,BMP2XMIT                                                      
         LA    R4,1(R4)                                                         
         CHI   R4,5                                                             
         BNH   DR10NX                                                           
         B     DRX                                                              
*                                                                               
DR50     CHI   R4,5                                                             
         BH    DRX                 BUMP TO NEXT LINE                            
         XC    8(30,R2),8(R2)      CLEAR DESC                                   
         BAS   RE,BMP2XMIT         XMIT AND BUMP 2 SCREEN FLDS                  
         MVI   8(R2),C' '          CLEAR REQ'D                                  
         BAS   RE,BMP2XMIT         XMIT AND BUMP 2 SCREEN FLDS                  
         MVI   8(R2),C' '          CLEAR EDIT RULE                              
         BAS   RE,BMP2XMIT         XMIT AND BUMP 2 SCREEN FLDS                  
         XC    8(2,R2),8(R2)       CLEAR LENGTH                                 
         BAS   RE,BMP2XMIT         XMIT AND BUMP 2 SCREEN FLDS                  
         LA    R4,1(R4)                                                         
         B     DR10NX                                                           
*                                                                               
DRX      EDIT  (P8,TOTLEN),(2,SUDTOTL)                                          
         OI    SUDTOTLH+6,X'80'                                                 
         B     EXIT                                                             
         DROP  R6                                                               
*                                                                               
**************************************************************                  
*  TRANSMIT AND BUMP TO NEXT FIELD                                              
**************************************************************                  
BMP2XMIT DS    0H                                                               
         OI    6(R2),X'80'         TRANSMIT                                     
         ZIC   R0,0(R2)            GET NEXT FIELD                               
         AR    R2,R0                                                            
         ZIC   R0,0(R2)            GET NEXT FIELD                               
         AR    R2,R0                                                            
         BR    RE                                                               
*                                                                               
**********************************************************************          
*                   DISPLAY KEY                                      *          
**********************************************************************          
*                                                                               
         USING SDURECD,R6                                                       
DK       L     R6,AIO                                                           
*                                                                               
         MVC   BYTE,SDUKAM         ISOLATE MEDIA CODE                           
         NI    BYTE,X'0F'                                                       
         LA    R5,MEDTAB           FIND MEDIA CODE USING MEDIA TABLE            
DK10     CLC   BYTE,1(R5)                                                       
         BE    DK20                                                             
         LA    R5,MEDTABLQ(R5)                                                  
         CLI   0(R5),X'FF'                                                      
         BNE   DK10                                                             
DK20     MVC   SUDMED,0(R5)                                                     
         OI    SUDMEDH+6,X'80'                                                  
         MVI   SUDMEDH+5,1          TRANSMIT MEDIA CODE TO SCREEN               
*                                                                               
         OC    SDUKCLT,SDUKCLT                                                  
         BNZ   DK30                                                             
         MVC   SUDCLT,=C'ALL'                                                   
         B     DK40                                                             
DK30     GOTO1 CLUNPK,DMCB,SDUKCLT,SUDCLT                                       
DK40     OI    SUDCLTH+6,X'80'                                                  
         MVI   SUDCLTH+5,3          TRANSMIT CLIENT CODE TO SCREEN              
         CLI   SUDCLT+2,C' '                                                    
         BH    *+8                                                              
         MVI   SUDCLTH+5,2                                                      
*                                                                               
DKX      B     VK                                                               
         DROP  R6                                                               
         EJECT                                                                  
******************************************************************              
*               LIST RECORDS                                     *              
******************************************************************              
*                                                                               
         USING SDURECD,R6                                                       
LR       LA    R6,KEY                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
         OC    KEY,KEY              TEST FIRST TIME                             
         BNZ   LR10                 KEY IS LAST RECORD READ                     
         MVC   KEY,SAVEKEY                                                      
         B     *+10                                                             
LR10     MVC   KEY,SVLSTKEY                                                     
*                                                                               
         GOTO1 HIGH                                                             
         B     LR30                                                             
LR20     GOTO1 SEQ                                                              
*                                                                               
LR30     CLC   KEY(3),KEYSAVE     TEST FOR ALL DONE                             
         BNE   LRX                                                              
*                                                                               
         OC    SUDCLT,SUDCLT       ANY CLIENT                                   
         BZ    LR40                                                             
         CLC   KEY+3(2),BCLT                                                    
         BNE   LRX                                                              
*                                                                               
LR40     GOTO1 GETREC              GET THE UCOM RECORD                          
         L     R6,AIO                                                           
*                                                                               
         MVC   LISTAR,SPACES                                                    
*                                                                               
         GOTO1 CLUNPK,DMCB,SDUKCLT,LSTCLT                                       
*                                                                               
         LA    R4,1                WHICH FIELD                                  
         LA    R2,LSTDESC1         START WITH 1                                 
         MVI   ELCODE,X'20'                                                     
*                                                                               
         USING SDUDELD,R6                                                       
LR50NX   L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         B     *+8                                                              
LR50     BAS   RE,NEXTEL                                                        
         BNE   LR55                                                             
         ZIC   R1,SDUDNUM                                                       
         CR    R1,R4                                                            
         BNE   LR50                                                             
*                                                                               
         MVC   0(10,R2),SDUDDESC   DESCRIPTION                                  
LR55     LA    R2,12(R2)           XMIT AND BUMP 2 SCREEN FLDS                  
         LA    R4,1(R4)                                                         
         CHI   R4,5                                                             
         BNH   LR50NX                                                           
*                                                                               
LR60     MVC   SVLSTKEY,KEY                                                     
*                                                                               
         GOTO1 LISTMON                                                          
         B     LR20                                                             
*                                                                               
LRX      DS    0H                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
******************************************************************              
* DELETE REC -                                                                  
******************************************************************              
RDEL     DS    0H                                                               
*                                                                               
         B     EXIT                YES - OK                                     
*                                                                               
******************************************************************              
* ERROR EXITS AND STUFF                                                         
******************************************************************              
*                                                                               
*                                                                               
ERRINV   MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
ERRMISS  MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
ERROVLEN MVC   ERRNUM,=AL2(310)                                                 
         B     SPERREX                                                          
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
         B     EXIT                                                             
*                                                                               
SPERREX  OI    GENSTAT2,USGETTXT                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTMSGNO,ERRNUM                                                   
         MVI   GTMTYP,GTMERR                                                    
         MVI   GTMSYS,23                                                        
VSFMERR  MVC   AIO,AIO1                                                         
         GOTO1 ERREX                                                            
         DROP  RF                                                               
*                                                                               
BADLNTH  EQU   303                                                              
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
*                                                                               
         DS    0F                                                               
ZEROES   DC    20C'0'                                                           
RELO     DS    F                                                                
*                                                                               
MEDTAB   DC   CL1'T',XL1'01'                                                    
MEDTABLQ EQU  *-MEDTAB                                                          
         DC   CL1'R',XL1'02'                                                    
         DC   CL1'N',XL1'03'                                                    
         DC   CL1'X',XL1'04'                                                    
         DC   CL1'C',XL1'08'                                                    
         DC   X'FF'                                                             
*                                                                               
         LTORG                                                                  
         SPACE 3                                                                
*                                                                               
         EJECT                                                                  
       ++INCLUDE SPSFMFFD                                                       
         EJECT                                                                  
         PRINT OFF                                                              
         ORG   CONTAGH                                                          
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM4CD         MAINT SCREEN                                  
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM4DD         LIST SCREEN                                   
         EJECT                                                                  
*                                                                               
       ++INCLUDE SPGENSDU                                                       
         EJECT                                                                  
       ++INCLUDE FAGETTXTD                                                      
         EJECT                                                                  
       ++INCLUDE SPSFMWORKD                                                     
         EJECT                                                                  
         ORG   SYSSPARE                                                         
*                                                                               
SAVEKEY  DS    CL32                                                             
SVLSTKEY DS    CL32                                                             
ERRNUM   DS    H                                                                
SAVER2   DS    F                                                                
SAVERE   DS    F                                                                
TOTLEN   DS    PL8                                                              
*                                                                               
         PRINT OFF                                                              
         SPACE 1                                                                
       ++INCLUDE SPGENPRG                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FATIOB                                                         
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LSTCLT   DS    CL3                                                              
         DS    CL1                                                              
LSTDESC1 DS    CL10                                                             
         DS    CL2                                                              
LSTDESC2 DS    CL10                                                             
         DS    CL2                                                              
LSTDESC3 DS    CL10                                                             
         DS    CL2                                                              
LSTDESC4 DS    CL10                                                             
         DS    CL2                                                              
LSTDESC5 DS    CL10                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003SPSFM84   08/25/04'                                      
         END                                                                    
