*          DATA SET NESFM10    AT LEVEL 091 AS OF 04/29/08                      
*PHASE T31C10A                                                                  
         TITLE 'T31C10  NETWORK COMMENT RECORD MAINTENANCE'                     
T31C10   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T31C10                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         LA    R7,1(RB)                                                         
         LA    R7,4095(R7)                                                      
         USING T31C10,RB,R7                                                     
         SPACE 3                                                                
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,PRINTREP       PRINT RECORDS                                
         BE    LR                                                               
         CLI   MODE,XRECADD        ADD PASSIVE POINTER                          
         BE    XA                                                               
         CLI   MODE,XRECDEL        DEL PASSIVE POINTER                          
         BE    XD                                                               
         CLI   MODE,XRECREST       REST PASSIVE POINTER                         
         BE    XR                                                               
EXIT     XIT1                                                                   
MAXPRD   EQU   252                                                              
         EJECT                                                                  
*     VALIDATE KEY ROUTINE                                                      
* BUILD KEY IN SVKEY2 / MOVE TO KEY AT EXIT                                     
* SVKEY WILL HAVE BINARY ZEROS, NOT FF'S. FOR FILTER PURPOSES.                  
         SPACE                                                                  
VK       DS    0H                                                               
         LA    R6,SVKEY2                                                        
         USING NCOMKEY,R6                                                       
         MVI   SVKEY2,X'FF'                  SET SVKEY2 TO FF'S                 
         MVC   SVKEY2+1(L'SVKEY2-1),SVKEY2                                      
         XC    SVKEY,SVKEY                                                      
         MVI   SVKEY2,X'0C'                                                     
         MVI   SVKEY,X'0C'                                                      
         SPACE                                                                  
         LA    R2,COMMEDH          * MEDIA                                      
         GOTO1 VALIMED                                                          
         MVC   NCOMKAM,BAGYMD                                                   
         MVC   SVKEY+1(1),BAGYMD                                                
         SPACE                                                                  
         MVI   NOPTFLG,1           SET OPTIONAL FLAG                            
         SPACE                                                                  
         LA    R2,COMCLTH             * CLIENT                                  
         XC    DMCB,DMCB                                                        
         GOTO1 VALIFLD                                                          
         BZ    VK10                                                             
         GOTO1 VALICLT                                                          
         MVC   NCOMKCLT,BCLT                                                    
         MVC   SVKEY+6(2),BCLT                                                  
         SPACE                                                                  
VK10     MVI   PRDFILT,C'N'           SET FILTER TO NO                          
         MVC   REQPRD,=CL3'POL'  SET DEFAULT                                    
         LA    R2,COMPRDH             * PRODUCT                                 
         XC    DMCB,DMCB                                                        
         GOTO1 VALIFLD                                                          
         BZ    VK20                                                             
         MVI   PRDFILT,C'Y'                                                     
         GOTO1 VALIPRD                                                          
         MVC   NCOMKPRD,BPRD                                                    
         CLI   BPRD,0                                                           
         BNE   *+16                                                             
         MVC   NCOMKAPR,COMPRD       ALPHA PROD FILLED ONLY ON OVERFLOW         
         OC    NCOMKAPR,SPACES       SPACE FILL                                 
         MVC   SVKEY+8(1),BPRD       BINARY PROD                                
         MVC   SVKEY+16(3),COMPRD   ALPHA PROD FILLED FOR LIST FILTER           
         MVC   REQPRD,COMPRD                                                    
         OI    REQPRD+2,X'40'                                                   
         SPACE                                                                  
VK20     LA    R2,COMESTH             * ESTIMATE                                
         XC    DMCB,DMCB                                                        
         GOTO1 VALIFLD,DMCB,255                                                 
         BZ    VK30                                                             
         GOTO1 VALIEST                                                          
         MVC   NCOMKEST,BEST                                                    
         MVC   SVKEY+9(1),BEST                                                  
         SPACE                                                                  
VK30     LA    R2,COMNTWKH            * NETWORK                                 
         GOTO1 VALIFLD                                                          
         BZ    VK40                                                             
         GOTO1 VALINTWK                                                         
         MVC   NCOMKNET,QNET                                                    
         MVC   SVKEY+10(4),QNET                                                 
         SPACE                                                                  
VK40     LA    R2,COMDPTH             * DAYPART                                 
         GOTO1 VALIFLD                                                          
         BZ    VK50                                                             
*                                                                               
         MVC   TEMPKEY,KEY                                                      
         GOTO1 VALIDPT                                                          
         MVC   NCOMKDPT,QDPT                                                    
         MVC   SVKEY+14(1),QDPT                                                 
         XC    KEY,KEY                                                          
         MVC   KEY(20),TEMPKEY                                                  
*                                                                               
VK50     LA    R2,COMPKGH             * PACKAGE                                 
         XC    DMCB,DMCB                                                        
         GOTO1 VALIFLD,DMCB,255                                                 
         BZ    VK60                                                             
         GOTO1 VALIPKG                                                          
         MVC   NCOMKPKG,BPAKG                                                   
         MVC   SVKEY+15(1),BPAKG                                                
         SPACE                                                                  
VK60     DS    0H                     * COMMENT ID                              
         CLI   ACTNUM,ACTLIST      IS IT LIST                                   
         BE    *+8                                                              
         MVI   NOPTFLG,0           NO/SET REQUIRED FIELD                        
         LA    R2,COMIDH                                                        
         XC    DMCB,DMCB                                                        
         GOTO1 VALIFLD,DMCB                                                     
         BZ    VKX                                                              
VK61     MVI   ERROR,INVALID                                                    
         LA    R3,3                                                             
         CR    R3,R1               R1 HAS LENGTH FROM VALIFLD                   
         BNE   TRAPERR             3 DIGITS FOR ID FILTER                       
         LR    R4,R2                                                            
VK62     CLI   8(R4),C'A'                                                       
         BL    VK64                                                             
         CLI   8(R4),C'9'                                                       
         BH    TRAPERR                                                          
         LA    R4,1(R4)                                                         
         B     VK65                                                             
VK64     CLI   ACTNUM,ACTLIST                                                   
         BNE   TRAPERR                                                          
         CLI   8(R4),C'*'          LIST ALLOWS * FOR FILTER                     
         BNE   TRAPERR                                                          
VK65     BCT   R3,VK62                                                          
         MVC   NCOMKID,8(R2)                                                    
         MVC   SVKEY+2(1),8(R2)                                                 
         MVC   NCOMKIDA,9(R2)                                                   
         MVC   SVKEY+3(1),9(R2)                                                 
         MVC   NCOMKIDB,10(R2)                                                  
         MVC   SVKEY+5(1),10(R2)                                                
         SPACE                                                                  
VKX      DS    0H                                                               
         LA    R2,COMMEDH                                                       
         MVC   KEY,SVKEY2                                                       
         B     EXIT                                                             
         EJECT                                                                  
* DISPLAY KEY                                                                   
         SPACE                                                                  
DK       DS    0H                                                               
         L     R6,AIO                                                           
         USING NCOMKEY,R6                                                       
         MVI   COMMED,C'N'                   * MEDIA                            
         FOUT  COMMEDH                                                          
         SPACE                                                                  
         CLC   NCOMKCLT,=4X'FF'                                                 
         BNE   *+14                                                             
         XC    COMCLT,COMCLT                                                    
         B     DK10                                                             
         GOTO1 CLUNPK,DMCB,(BCLIAAN,NCOMKCLT),COMCLT   * CLIENT                 
DK10     FOUT  COMCLTH                                                          
*                                                                               
         CLI   NCOMKAPR,X'FF'                * PRODUCT                          
         BE    DK11                                                             
         MVC   COMPRD(3),NCOMKAPR                                               
         B     DK16                                                             
         SPACE                                                                  
DK11     CLI   NCOMKPRD,X'FF'                * PRODUCT                          
         BE    DK15                                                             
         CLC   NCOMKCLT,BCLT                                                    
         BE    DK12                                                             
         MVC   BCLT,NCOMKCLT       SET NEW CLT CODE                             
         BAS   RE,GETCLTRC         GET CLTHDR,SVCLIST                           
         B     DK14                                                             
DK12     CLC   BPRD,NCOMKPRD                                                    
         BE    DK14A                                                            
DK14     MVC   BPRD,NCOMKPRD       SET NEW PRD CODE                             
         BAS   RE,GETPRD           GETS QPRD                                    
DK14A    MVC   COMPRD,QPRD                                                      
         B     *+10                                                             
DK15     MVC   COMPRD(3),=C'POL'                                                
DK16     FOUT  COMPRDH                                                          
         SPACE                                                                  
         CLI   NCOMKEST,X'FF'      * ESTIMATE                                   
         BNE   *+14                                                             
         XC    COMEST,COMEST                                                    
         B     DK20                                                             
         EDIT  (B1,NCOMKEST),(3,COMEST),ALIGN=LEFT                              
DK20     FOUT  COMESTH                                                          
         SPACE                                                                  
         CLI   NCOMKNET,X'FF'      * NETWORK                                    
         BNE   *+14                                                             
         XC    COMNTWK,COMNTWK                                                  
         B     DK30                                                             
         MVC   COMNTWK,NCOMKNET                                                 
DK30     FOUT  COMNTWKH                                                         
*&&DO                                                                           
         CLI   NCOMKDPT,X'FF'      * DAYPART                                    
         BNE   *+14                                                             
         XC    COMDPT,COMDPT                                                    
         B     DK40                                                             
         MVC   COMDPT(1),NCOMKDPT                                               
*&&                                                                             
*                                                                               
         MVC   TEMPKEY,KEY                                                      
         XC    COMDPT,COMDPT                                                    
         CLI   NCOMKDPT,X'FF'                                                   
         BE    DK40                                                             
*                                                                               
         MVC   BCLT,NCOMKCLT                                                    
         GOTO1 VALIDPT,DMCB,(X'01',NCOMKDPT)                                    
         CLI   QDPT,0                                                           
         BE    DK40                                                             
         MVC   COMDPT(2),QDPT2     MOVE IN ALPHA DAYPART                        
         XC    KEY,KEY                                                          
         MVC   KEY(20),TEMPKEY                                                  
         GOTO1 HIGH                                                             
*                                                                               
DK40     FOUT  COMDPTH                                                          
         SPACE                                                                  
         CLI   NCOMKPKG,X'FF'      * PACKAGE                                    
         BNE   *+14                                                             
         XC    COMPKG,COMPKG                                                    
         B     DK50                                                             
         EDIT  (B1,NCOMKPKG),(3,COMPKG),ALIGN=LEFT                              
DK50     FOUT  COMPKGH                                                          
         SPACE                                                                  
         XC    COMID,COMID                                                      
         MVC   COMID(1),NCOMKID               * COMMENT ID                      
         MVC   COMID+1(1),NCOMKIDA                                              
         MVC   COMID+2(1),NCOMKIDB                                              
         FOUT  COMIDH                                                           
         SPACE                                                                  
         DROP  R6                                                               
         B     EXIT                                                             
         EJECT                                                                  
* VALIDATE RECORD                                                               
VR       MVI   ELCODE,X'04'                                                     
         GOTO1 REMELEM                                                          
         XC    ELEM,ELEM                                                        
         LA    RE,ELEM                                                          
         USING NSUPEL,RE                                                        
         MVI   NSUPELEM,X'04'                                                   
         MVI   NSUPELEN,20                                                      
         MVC   NSUPPROD,REQPRD     ALPHA PRODUCT CODE                           
         OC    NSUPPROD,SPACES                                                  
         GOTO1 ADDELEM                                                          
*                                                                               
         DS    0H                                                               
         MVI   ELCODE,X'02'                                                     
         GOTO1 REMELEM                                                          
         LA    R2,COMCOMH                                                       
         CLI   5(R2),0                                                          
         BNE   *+12                                                             
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
         SR    R4,R4            RUN DOWN SCREEN AND GET NUM OF LINES            
         LA    R4,1                 INPUT AND SET IN R4 AS BCT LIMIT            
VR4      ST    R4,FULL                                                          
VR5      ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),9                                                          
         BNH   VR8                                                              
         LA    R4,1(R4)                                                         
         CLI   5(R2),0                                                          
         BE    VR5                                                              
         B     VR4                                                              
VR8      LA    R2,COMCOMH                                                       
         L     R4,FULL                                                          
         LA    R3,1                                                             
VR10     XC    ELEM,ELEM                                                        
         MVI   ELEM,X'02'                                                       
         STC   R3,ELEM+2           SET COMMENT LINE NUMBER                      
         ZIC   R1,5(R2)                                                         
         LTR   R1,R1                                                            
         BNZ   *+8                                                              
         LA    R1,1                                                             
         LA    R1,3(R1)                                                         
         STC   R1,ELEM+1           SET ELEMENT LENGTH                           
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELEM+3(0),8(R2)                                                  
         GOTO1 ADDELEM                                                          
         SPACE                                                                  
         LA    R3,1(R3)                                                         
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),9             TEST END OF SCREEN                           
         BNH   VRX                                                              
         BCT   R4,VR10                                                          
*                                                                               
         SPACE                                                                  
VRX      B     EXIT                                                             
         EJECT                                                                  
* DISPLAY RECORD                                                                
         SPACE                                                                  
DR       DS    0H                        *  CLEAR SCREEN                        
         LA    R2,COMCOMH                                                       
DR2      OC    8(L'COMCOM,R2),SPACES                                            
         CLC   8(L'COMCOM,R2),SPACES                                            
         BE    DR5                                                              
         XC    8(L'COMCOM,R2),8(R2)                                             
         FOUT  (R2)                                                             
DR5      ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),9                                                          
         BNH   DR7                                                              
         B     DR2                                                              
         SPACE                                                                  
DR7      LA    R2,COMCOMH                                                       
         MVC   AIO,AIO1                                                         
         L     R6,AIO                                                           
         LA    R6,27(R6)                                                        
         USING NCOMELEM,R6                                                      
         MVI   ELCODE,X'02'                                                     
         CLI   0(R6),X'02'                                                      
         BE    DR12                                                             
         BAS   RE,GETEL                                                         
         BE    DR12                                                             
         DC    H'0'                                                             
DR12     ZIC   R1,NCOMELEN                                                      
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),NCOMETXT                                                 
         FOUT  (R2)                                                             
         BAS   RE,NEXTEL                                                        
         BNE   DRX                                                              
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),9                                                          
         BH    DR12                                                             
*                                                                               
DRX      DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ADD PASSIVE POINTER                                                           
***********************************************************************         
XA       DS    0H             RECORD HAS BEEN ADDED/ADD PASSIVE POINTER         
         L     R6,AIO                                                           
         USING NCOMRECD,R6                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(20),0(R6)                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(20),KEYSAVE                                                  
         BE    XA20                                                             
*                                                                               
         L     RF,ACOMFACS                 IF TEST SYSTEM ?                     
         L     RF,CGETFACT-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,0                                                      
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         CLI   FASYSID,1                                                        
         BE    EXIT                SKIP/SINCE THIS BRINGS DOWN TEST             
         DC    H'0'                                                             
         DROP  R1                                                               
                                                                                
XA20     MVC   WORK(4),KEY+21      SAVE DISKADDRESS                             
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'8C'                                                        
         MVC   KEY+1(7),NCOMKAM                                                 
         MVC   KEY+8(3),REQPRD                                                  
         MVC   KEY+11(7),NCOMKEST                                               
         MVC   KEY+18(2),=XL2'FFFF'                                             
         MVC   KEY+21(4),WORK      INSERT DISK ADDRESS                          
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'UNTDIR  ',KEY,KEY                      
XAX      B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* DELETE PASSIVE POINTER                                                        
***********************************************************************         
XD       DS    0H             RECORD HAS BEEN ADDED/ADD PASSIVE POINTER         
         L     R6,AIO                                                           
         USING NCOMRECD,R6                                                      
*                                                                               
         OI    DMINBTS,X'08'                                                    
         XC    KEY,KEY                                                          
         MVI   KEY,X'8C'                                                        
         MVC   KEY+1(7),NCOMKAM                                                 
         MVC   KEY+8(3),REQPRD                                                  
         MVC   KEY+11(7),NCOMKEST                                               
         MVC   KEY+18(2),=XL2'FFFF'                                             
         GOTO1 HIGH                                                             
         CLC   KEY(20),KEYSAVE                                                  
         BNE   XDX                                                              
         OI    KEY+20,X'80'                                                     
         GOTO1 WRITE                                                            
XDX      B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* DELETE PASSIVE POINTER                                                        
***********************************************************************         
XR       DS    0H             RECORD HAS BEEN ADDED/ADD PASSIVE POINTER         
         L     R6,AIO                                                           
         USING NCOMRECD,R6                                                      
*                                                                               
         OI    DMINBTS,X'08'                                                    
         XC    KEY,KEY                                                          
         MVI   KEY,X'8C'                                                        
         MVC   KEY+1(7),NCOMKAM                                                 
         MVC   KEY+8(3),REQPRD                                                  
         MVC   KEY+11(7),NCOMKEST                                               
         MVC   KEY+18(2),=XL2'FFFF'                                             
         GOTO1 HIGH                                                             
         CLC   KEY(20),KEYSAVE                                                  
         BNE   XRX                                                              
         NI    KEY+20,X'7F'                                                     
         GOTO1 WRITE                                                            
XRX      B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* LIST RECORDS *                                                                
         SPACE                                                                  
LR       DS    0H                                                               
         CLI   MODE,PRINTREP                                                    
         BNE   LR02                                                             
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         LA    R1,HDRTN                                                         
         ST    R1,HEADHOOK                                                      
LR02     MVI   NLISTS,X'0A'        SET NUM OF LIST LINES                        
         MVC   AIO,AIO1                                                         
*                                                                               
         LA    R2,SVKEY                                                         
         LA    R1,13                   LIST ALLOWS * IN ID FOR FILTER           
CLEAR    CLI   0(R2),C'*'              SET THESE TO 0                           
         BNE   *+8                                                              
         MVI   0(R2),0                                                          
         LA    R2,1(R2)                                                         
         BCT   R1,CLEAR                                                         
*                                                                               
         LA    R3,KEY                                                           
         USING NCOMKEY,R3                                                       
         OC    KEY(13),KEY                                                      
         BNZ   LR05                                                             
         MVC   KEY,SVKEY           USE SVKEY SINCE IT HAS ZEROS                 
*                                                                               
LR05     GOTO1 HIGH               (REMEMBER SVKEY2 HAS FF)                      
         B     LR22                                                             
         SPACE                                                                  
LR20     GOTO1 SEQ                                                              
         LA    R3,KEY                                                           
*                                                                               
LR22     DS    0H                                                               
         CLC   SVKEY(2),KEY                                                     
         BNE   LRX                                                              
         CLI   NCOMKIDA+1,X'FF'     THIS IS UNUSED BYTE/SHOULD=FF               
         BNE   LR20                (TO KEEP OLD COM RECS OUT OF LIST)           
         CLC   SVKEY+2(4),=6X'00'  COMMENT ID                                   
         BE    LR22H                                                            
         CLI   SVKEY+2,0           NCOMKID                                      
         BE    LR22A                                                            
         CLC   SVKEY+2(1),NCOMKID                                               
         BL    LR20                                                             
         BH    LRX                                                              
LR22A    CLI   SVKEY+3,0           NCOMKIDA                                     
         BE    LR22B                                                            
         CLC   SVKEY+3(1),NCOMKIDA                                              
         BNE   LR20                                                             
LR22B    CLI   SVKEY+5,0           NCOMKIDB                                     
         BE    LR22H                                                            
         CLC   SVKEY+5(1),NCOMKIDB                                              
         BNE   LR20                                                             
LR22H    CLC   SVKEY+6(2),=6X'00'  CLIENT                                       
         BE    *+14                                                             
         CLC   SVKEY+6(2),NCOMKCLT                                              
         BNE   LR20                                                             
*                                                                               
         CLI   PRDFILT,C'Y'         ARE WE FILTERING ON PRODUCT                 
         BNE   LR22K                                                            
         CLI   NCOMKAPR,X'FF'       SOULD WE CHECK ALPHA                        
         BE    LR22I                                                            
         CLC   SVKEY+16(3),NCOMKAPR TRY FOR MATCH ON ALPHA                      
         BE    LR22K                                                            
         B     LR20                                                             
LR22I    CLC   SVKEY+8(1),NCOMKPRD  THEN TRY FOR MATCH ON PROD EQUATE           
         BNE   LR20                                                             
*                                                                               
LR22K    CLC   SVKEY+9(1),=6X'00' ESTIMATE                                      
         BE    *+14                                                             
         CLC   SVKEY+9(1),NCOMKEST                                              
         BNE   LR20                                                             
         CLC   SVKEY+10(4),=6X'00' NETWORK                                      
         BE    *+14                                                             
         CLC   SVKEY+10(4),NCOMKNET                                             
         BNE   LR20                                                             
         CLC   SVKEY+14(1),=6X'00' DAYPART                                      
         BE    *+14                                                             
         CLC   SVKEY+14(1),NCOMKDPT                                             
         BNE   LR20                                                             
         CLC   SVKEY+15(1),=6X'00' PACKAGE                                      
         BE    *+14                                                             
         CLC   SVKEY+15(1),NCOMKPKG                                             
         BNE   LR20                                                             
         SPACE                                                                  
*                                                                               
         GOTO1 GETREC                                                           
         SPACE                                                                  
*                                                                               
         MVC   SYSFIL,=C'UNTFIL  '                                              
         MVC   SYSDIR,=C'UNTDIR  '                                              
*                                                                               
         LA    R3,KEY                                                           
         CLC   NCOMKCLT,=XL2'FFFF'                                              
         BE    LR23                                                             
*&&DO                                                                           
         MVC   BCLT,NCOMKCLT                                                    
         GOTO1 VLMTDACC                                                         
*&&                                                                             
         GOTO1 VLMTDACC,DMCB,NCOMKAM,NCOMKCLT                                   
         CLI   DMCB+4,X'FF'                                                     
         BE    LR20                                                             
*                                                                               
LR23     DS    0H                                                               
         L     R3,AIO                                                           
*                                                                               
         LA    R5,LISTAR                                                        
         USING PCOMID,R5                                                        
         XC    LISTAR,LISTAR                                                    
         L     R6,AIO                                                           
         USING NCOMELEM,R6                                                      
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   PCOMID,NCOMKID               * COMMENT ID                        
         MVC   PCOMIDA,NCOMKIDA                                                 
         MVC   PCOMIDB,NCOMKIDB                                                 
         SPACE                                                                  
         CLC   NCOMKCLT,=4X'FF'                                                 
         BE    LR24A                                                            
         GOTO1 CLUNPK,DMCB,(BCLIAAN,NCOMKCLT),PCLT    * CLIENT                  
         SPACE                                                                  
LR24A    CLI   NCOMKAPR,X'FF'       IS ALPHA PRODUCT IN KEY                     
         BE    PRD3                                                             
         MVC   PPRD,NCOMKAPR       * PRODUCT                                    
         B     LR24B                                                            
PRD3     CLI   NCOMKPRD,X'FF'       GET BINARY PRODUCT                          
         BE    LR24AA                                                           
         CLC   NCOMKCLT,BCLT                                                    
         BE    PRD5                                                             
         MVC   BCLT,NCOMKCLT       SET NEW BCLT                                 
         BAS   RE,GETCLTRC         GETS SVCLIST                                 
         B     PRD7                                                             
PRD5     CLC   BPRD,NCOMKPRD                                                    
         BE    PRD8                                                             
PRD7     MVC   BPRD,NCOMKPRD       SET NEW BPRD                                 
         BAS   RE,GETPRD           GETS QPRD                                    
PRD8     MVC   PPRD,QPRD                                                        
         B     LR24B                                                            
LR24AA   MVC   PPRD(3),=C'POL'                                                  
         SPACE                                                                  
LR24B    CLI   NCOMKEST,X'FF'      * ESTIMATE                                   
         BE    LR24C                                                            
         EDIT  (B1,NCOMKEST),(3,PEST)                                           
         SPACE                                                                  
LR24C    CLC   NCOMKNET,=4X'FF'    * NETWORK                                    
         BE    LR24D                                                            
         MVC   PNTWK,NCOMKNET                                                   
*                                                                               
LR24D    DS    0H                                                               
         CLI   NCOMKDPT,X'FF'                                                   
         BE    LR24E                                                            
*                                                                               
         MVC   TEMPKEY,KEY                                                      
         CLC   NCOMKCLT,=XL2'FFFF'                                              
         BE    *+10                                                             
         MVC   BCLT,NCOMKCLT                                                    
         GOTO1 VALIDPT,DMCB,(X'01',NCOMKDPT)                                    
         CLI   QDPT,0                                                           
         BNE   *+6                                                              
         DC    H'00'                                                            
*                                                                               
         MVC   PDPT,QDPT2          MOVE IN ALPHA DAYPART                        
         XC    KEY,KEY                                                          
         MVC   KEY(20),TEMPKEY                                                  
         GOTO1 HIGH                                                             
*                                                                               
*&&DO                                                                           
LR24D    CLI   NCOMKDPT,X'FF'      * DAYPART                                    
         BE    LR24E                                                            
         MVC   PDPT(1),NCOMKDPT                                                 
*&&                                                                             
         SPACE                                                                  
LR24E    CLI   NCOMKPKG,X'FF'      * PACKAGE                                    
         BE    LR24F                                                            
         EDIT  (B1,NCOMKPKG),(3,PPKG)                                           
         SPACE                                                                  
LR24F    CLI   MODE,PRINTREP                                                    
         BE    LR25                                                             
         ZIC   R1,NCOMELEN                                                      
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PTEXT(0),NCOMETXT      TEXT TO LIST LINE                         
         GOTO1 LISTMON                                                          
         B     LR20                GOTO READ SEQ                                
         DROP  R5                                                               
         SPACE                                                                  
* PRINTING THE LINE                                                             
LR25     LA    R4,P+10                                                          
         USING PLINED,R4                                                        
         MVC   PCOMID(PTEXT-PCOMID),LISTAR   KEY DATA FROM LIST TO P            
         MVI   PMED,C'N'                   * MEDIA                              
         L     R6,AIO                                                           
         USING NCOMELEM,R6                                                      
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
LR28     ZIC   R1,NCOMELEN                                                      
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PTEXT(0),NCOMETXT                TEXT                            
         GOTO1 SPOOL,DMCB,(R8)                                                  
         BAS   RE,NEXTEL           ARE THERE MORE TEXT ELEMS                    
         BE    LR28                YES/GET THEM AND PRINT                       
         GOTO1 SPOOL,DMCB,(R8)     NO/SKIP LINE BETWEEN RECS                    
         B     LR20                GET NEXT RECORD                              
         SPACE                                                                  
LRX      DS    0H                                                               
         B     EXIT                                                             
         DROP  R3,R4,R6                                                         
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
HEADING  DS    0H                                                               
         SSPEC H1,3,REQUESTOR                                                   
         SSPEC H1,46,C'NETWORK COMMENT RECORDS'                                 
         SSPEC H2,46,C'-----------------------'                                 
         SSPEC H1,93,AGYNAME                                                    
         SSPEC H2,93,AGYADD                                                     
         SSPEC H3,93,REPORT                                                     
         SSPEC H4,93,RUN                                                        
         SSPEC H5,103,PAGE                                                      
         DC    X'00'                                                            
*                                                                               
HDRTN    NTR1                                                                   
         LA    R2,H8+10                                                         
         USING PLINED,R2                                                        
         MVC   PMED-2(3),=C'MED'                                                
         MVC   PMED-2+132(3),=50C'-'                                            
         MVC   PCOMID(3),=C'ID#'                                                
         MVC   PCOMID+132(3),=50C'-'                                            
         MVC   PCLT,=C'CLT'                                                     
         MVC   PCLT+132(3),=50C'-'                                              
         MVC   PPRD,=C'PRD'                                                     
         MVC   PPRD+132(3),=50C'-'                                              
         MVC   PEST,=C'EST'                                                     
         MVC   PEST+132(3),=50C'-'                                              
         MVC   PNTWK,=C'NTWK'                                                   
         MVC   PNTWK+132(4),=50C'-'                                             
         MVC   PDPT(3),=C'DPT'                                                  
         MVC   PDPT+132(3),=50C'-'                                              
         MVC   PPKG(3),=C'PKG'                                                  
         MVC   PPKG+132(3),=50C'-'                                              
         MVC   PTEXT(7),=C'COMMENT'                                             
         MVC   PTEXT+132(50),=50C'-'                                            
         B     EXIT                                                             
         EJECT                                                                  
         SPACE 3                                                                
INITSPT  NTR1  0H                                                               
         MVC   SYSDIR,=C'SPTDIR  '                                              
         MVC   SYSFIL,=C'SPTFILE '                                              
         MVC   DATADISP,=H'24'                                                  
         MVC   LKEY,=H'13'                                                      
         B     EXIT                                                             
         SPACE 3                                                                
INITUNT  NTR1                                                                   
         MVC   SYSDIR,=C'UNTDIR  '                                              
         MVC   SYSFIL,=C'UNTFIL  '                                              
         MVC   DATADISP,=H'27'                                                  
         MVC   LKEY,=H'20'                                                      
         B     EXIT                                                             
         SPACE 3                                                                
GETCLTRC NTR1                                                                   
         BAS   RE,INITSPT                                                       
         MVC   SAVEKEY(30),KEY                                                  
         MVC   SAVEKEY+40(4),DMDSKADD    SAVE DMDSKADD FOR LIST                 
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO2                                                         
         USING CLTHDR,R6                                                        
         GOTO1 GETREC                                                           
         MOVE  (SVCLIST,880),CLIST                                              
         MOVE  (SVCLIST2,140),CLIST2                                            
         MVC   AIO,AIO1            RESET IO AREA                                
         BAS   RE,INITUNT          RESET UNIT FILE                              
         MVC   KEY(30),SAVEKEY         RESET KEY                                
         GOTO1 HIGH                RESET FOR SEQ                                
         MVC   DMDSKADD,SAVEKEY+40 RESET DMDSKADD(FOR LIST)                     
         B     EXIT                                                             
         SPACE 2                                                                
GETPRD   NTR1                                                                   
         LA    R2,SVCLIST                                                       
         LA    R5,MAXPRD           MAX NUMBER OF PRODUCTS                       
GP5      CLI   3(R2),0             IF E-O-F CLIST                               
         BE    GPX                 SET TO UNDEFINED                             
         CLC   3(1,R2),BPRD                                                     
         BNE   GP10                                                             
         MVC   QPRD,0(R2)          SET 3 CHAR PRINTABLE PRD CODE                
         B     GPX                                                              
GP10     LA    R2,4(R2)            INCREMENT CLIST                              
         BCT   R5,GP5                                                           
GPX      B     EXIT                                                             
         EJECT                                                                  
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
         B     EXIT                                                             
*                                                                               
         LTORG                                                                  
         SPACE 3                                                                
*                                                                               
         SPACE                                                                  
PLINED   DSECT                                                                  
         DS    CL2                                                              
PMED     DS    CL1                                                              
         DS    CL1                                                              
PCOMID   DS    CL1                                                              
PCOMIDA  DS    CL1                                                              
PCOMIDB  DS    CL1                                                              
         DS    CL1                                                              
PCLT     DS    CL3                                                              
         DS    CL1                                                              
PPRD     DS    CL3                                                              
         DS    CL1                                                              
PEST     DS    CL3                                                              
         DS    CL1                                                              
PNTWK    DS    CL4                                                              
         DS    CL1                                                              
PDPT     DS    CL2                                                              
         DS    CL2                                                              
PPKG     DS    CL1                                                              
         DS    CL3                                                              
PTEXT    DS    CL50                                                             
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE DDCOMFACS                                                      
*                                                                               
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDFLDIND                                                       
         EJECT                                                                  
       ++INCLUDE NESFMFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NESFMF1D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NESFMF2D                                                       
         EJECT                                                                  
       ++INCLUDE NESFMWORKD                                                     
         ORG   SYSSPARE                                                         
*                           *******  T31C10 WORK AREA  *******                  
WORKAREA DS    0CL100                                                           
SVKEY2   DS    CL48                                                             
SAVEKEY  DS    CL48                                                             
TEMPKEY  DS    XL20                                                             
REQPRD   DS    CL3                                                              
PRDFILT  DS    CL1                  Y=PROD FILTER ON LIST                       
         EJECT                                                                  
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE NEGENCOM                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'091NESFM10   04/29/08'                                      
         END                                                                    
