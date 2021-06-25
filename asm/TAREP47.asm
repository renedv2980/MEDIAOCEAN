*          DATA SET TAREP47    AT LEVEL 087 AS OF 05/27/03                      
*PHASE T70347A,*                                                                
         TITLE 'T70347 - QUESTIONNARE REPORT'                                   
T70347   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70347                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC             RC=A(CONTROLLER W/S)                         
         L     RA,ATWA                                                          
         USING T703FFD,RA          RA=A(SCREEN)                                 
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8           R8=A(SPOOL AREA)                             
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          R9=A(SYSTEM W/S)                             
         LA    R6,BUFF                                                          
         LA    R6,8(R6)                                                         
         USING QUESD,R6            R6=A(LOCAL W/S)                              
         EJECT                                                                  
*                                                                               
*              MODE CONTROLLED ROUTINES                                         
*                                                                               
         GOTO1 INITIAL,DMCB,0      INITIALIZE                                   
*                                                                               
         CLI   MODE,VALKEY         HANDLE ONLY MODE VALIDATE SCREEN             
         BNE   XIT                                                              
         BAS   RE,VKEY             VALIDATE SCREEN FIELDS                       
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE SCREEN                                                  
*                                                                               
VKEY     NTR1                                                                   
         LR    RE,R6               CLEAR LOCAL W/S                              
         LA    RF,QUESLNQ                                                       
         XCEFL ,                                                                
*                                                                               
         LH    RF,=AL2(TIEND-TASYSIOD)                                          
         XCEFL TASYSIOD,(RF)                                                    
*                                                                               
         XC    TIFILTS,TIFILTS     CLEAR SYSIO FILTERS                          
*                                                                               
         XC    SQUMEDN,SQUMEDN     PRE CLEAR NAME FIELDS                        
         OI    SQUMEDNH+6,X'80'                                                 
         XC    SQUEMPN,SQUEMPN                                                  
         OI    SQUEMPNH+6,X'80'                                                 
         XC    SQUAGYN,SQUAGYN                                                  
         OI    SQUAGYNH+6,X'80'                                                 
         XC    SQUCLTN,SQUCLTN                                                  
         OI    SQUCLTNH+6,X'80'                                                 
*                                                                               
         LA    R2,SQUPDH           R2=A(PERIOD FIELD)                           
         GOTO1 ANY                                                              
         BAS   RE,VALPD            VALIDATE PERIOD                              
         MVI   TIQDTYPE,TIQDBILL   PERIOD IS BILL DATE                          
         MVC   TIQPSTR,QUPSTA      START DATE                                   
         MVC   TIQPEND,QUPEND      END DATE                                     
*                                                                               
         LA    R2,SQUMEDH          R2=A(MEDIA FIELD)                            
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
         GOTO1 MEDVAL,DMCB,8(R2)                                                
         BNE   INVERR                                                           
         CLI   8(R2),TACOMEDP      DON'T ALLOW PRINT ON THIS SCREEN             
         BE    INVERR                                                           
         MVC   TIFMED,8(R2)                                                     
         MVC   SQUMEDN,TGMENAME                                                 
*                                                                               
         LA    R2,SQUEMPH          R2=A(EMPLOYER FIELD)                         
         CLI   5(R2),0                                                          
         BE    VK10                                                             
         GOTO1 RECVAL,DMCB,TLEMCDQ,(X'08',(R2)),SQUEMPNH                        
         MVC   TIFEMP,TGEMP                                                     
*                                                                               
VK10     LA    R2,SQUAGYH          R2=A(AGENCY FIELD)                           
         CLI   5(R2),0                                                          
         BE    VK20                                                             
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'08',(R2)),SQUAGYNH                        
         MVC   TIFAGY,TGAGY                                                     
*                                                                               
VK20     LA    R2,SQUCLTH          R2=A(CLIENT FIELD)                           
         CLI   5(R2),0                                                          
         BE    VK30                                                             
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'08',(R2)),SQUCLTNH                        
         MVC   TIFCLI,TGCLI        CLIENT FILTER                                
*                                                                               
VK30     LA    R2,SQUCTYFH         R2=A(COMMERCIAL TYPE FILTER)                 
         MVI   MYCTYPE,0                                                        
         MVI   MYBYTE,0                                                         
         CLI   5(R2),0                                                          
         BE    VK70                                                             
         LA    R3,8(R2)                                                         
         CLI   5(R2),1             IF L'1, MUST BE A RECORD TYPE                
         BE    VK40                                                             
         CLI   5(R2),2             IF LENGTH IS 2                               
         BNE   VK31                                                             
         LA    R3,9(R2)                                                         
         CLI   8(R2),C'@'          MUST BE FLIST                                
         BE    VK35                                                             
         CLI   8(R2),C'-'          OR NEGATIVE                                  
         BNE   INVERR              ELSE ERROR                                   
         MVI   MYBYTE,C'N'         SET NEGATIVE FLAG                            
         B     VK40                                                             
*                                                                               
VK31     CLC   8(2,R2),=C'-@'      LENGTH IS 3, MUST BE NEGATIVE FLIST          
         BE    VK32                                                             
         CLC   8(2,R2),=C'@-'                                                   
         BNE   INVERR                                                           
VK32     MVI   MYBYTE,C'N'         SET NEGATIVE FLAG                            
         LA    R3,10(R2)                                                        
*                                                                               
VK35     MVI   TGLTYP,TLGLTYPF                                                  
         MVC   MYDUB,SPACES                                                     
         MVC   MYDUB(1),0(R3)                                                   
         GOTO1 RECVAL,DMCB,TLGLCDQ,(X'80',MYDUB)                                
         BNE   INVERR                                                           
         MVC   MYCTYPE,TGLST                                                    
         NI    MYCTYPE,X'FF'-X'80'   TURN OFF 80 BIT FOR FLIST                  
         B     VK60                                                             
*                                                                               
*                                                                               
VK40     GOTO1 CTYPVAL,DMCB,0(R3)                                               
         BNE   INVERR                                                           
*                                                                               
VK50     MVC   MYCTYPE,0(R3)                                                    
*                                                                               
VK60     CLI   MYBYTE,C'N'         IS THIS NEGATIVE                             
         BNE   *+8                                                              
         NI    MYCTYPE,X'FF'-X'40'   TURN OFF 40 BIT                            
         MVC   TIFCTYPE,MYCTYPE    SET COMML TYPE FILTER                        
*                                                                               
VK70     LA    R2,SQUCMNLH         R2=A(COMMERCIAL MIN LENGTH)                  
         MVI   QUCLNMN,0                                                        
         CLI   5(R2),0                                                          
         BE    VK80                                                             
         GOTO1 VALINUM                                                          
         MVC   QUCLNMN,ACTUAL                                                   
*                                                                               
VK80     LA    R2,SQUCMXLH         R2=A(COMMERCIAL MAX LENGTH)                  
         MVI   QUCLNMX,0                                                        
         CLI   5(R2),0                                                          
         BE    VK90                                                             
         GOTO1 VALINUM                                                          
         MVC   QUCLNMX,ACTUAL                                                   
*                                                                               
VK90     LA    R2,SQUOPTH          R2=A(OPTIONS FIELD)                          
         BAS   RE,VALOPT           VALIDATE OPTIONS                             
*                                                                               
VKX      B     XIT                                                              
         EJECT                                                                  
*              ROUTINE VALIDATES PERIOD FIELD                                   
*                                  NTRY (R2) = A(PERIOD FIELD)                  
         SPACE                                                                  
VALPD    NTR1                                                                   
         LA    R3,BLOCK            R3=A(BLOCK TO PASS)                          
         USING PERVALD,R3                                                       
         GOTO1 PDVAL,DMCB,(R3)     GO TO SYSTEM PERIOD VAL. ROUTINE             
*                                                                               
         MVC   QUPERIOD,PVALCPER   SAVE DISPLAYABLE PERIOD IN W/S               
         MVC   QUPSTA,PVALPSTA     SAVED PACKED START DATE                      
         MVC   QUPEND,PVALPEND     SAVED PACKED END DATE                        
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
*              ROUTINE TO VALIDATE OPTIONS                                      
*                                  NTRY (R2) = A(OPTIONS FIELD)                 
*                                                                               
VALOPT   NTR1                                                                   
         CLI   5(R2),0             IF OPTIONS FIELD INPUT                       
         BE    VOPTX                                                            
*                                                                               
         XC    BLOCK,BLOCK         CLEAR SCAN BLOCK                             
         LA    R3,BLOCK            R3=A(SCAN BLOCK)                             
         USING SCAND,R3                                                         
         GOTO1 SCANNER,DMCB,(R2),(R3)                                           
         CLI   4(R1),0                                                          
         BE    INVERR                                                           
         ZIC   R0,4(R1)            R0=N'SCAN BLOCK ENTRIES                      
*                                                                               
VOPT10   CLC   =C'TRACE',SCDATA1   TRACE                                        
         BNE   VOPT20                                                           
         OI    QUOPTS,QUTRACE                                                   
         B     VOPT100                                                          
*                                                                               
VOPT20   CLC   =C'COM',SCDATA1     INTERNAL COMMERCIAL # FILTER                 
         BNE   VOPT30                                                           
         ZIC   R4,SCLEN2                                                        
         CH    R4,=H'8'                                                         
         BNE   INVERR              INPUT LENGTH MUST BE 8                       
         GOTO1 HEXIN,DMCB,SCDATA2,MYCOM,(R4)                                    
         OC    DMCB+12(4),DMCB+12                                               
         BZ    INVERR              INVALID IF 4TH PARAMETER IS 0                
         B     VOPT100                                                          
*                                                                               
VOPT30   CLC   =C'CID',SCDATA1     COMMERCIAL ID                                
         BNE   VOPT40                                                           
         MVC   MYCID(10),SCDATA2                                                
         OC    MYCID,SPACES                                                     
         B     VOPT100                                                          
*                                                                               
VOPT40   CLC   =C'CLA',SCDATA1     ONLY WANT COMM'LS WITH CLA ACTIVITY          
         BNE   VOPT50                                                           
         OI    QUOPTS,QUCLA                                                     
         B     VOPT100                                                          
*                                                                               
VOPT50   B     INVERR                                                           
*                                                                               
VOPT100  LA    R3,SCANNEXT         BUMP TO NEXT                                 
         BCT   R0,VOPT10           AND CONTINUE                                 
*                                                                               
VOPTX    B     XIT                                                              
         EJECT                                                                  
*              ERRORS, EXITS                                                    
         SPACE 2                                                                
MISSERR  MVI   ERROR,MISSING       MISSING INPUT FIELD                          
         B     THEEND                                                           
*                                                                               
INVERR   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     THEEND                                                           
*                                                                               
THEEND   GOTO1 ERREX                                                            
*                                                                               
YES      SR    RC,RC               SET CONDITION CODES                          
NO       LTR   RC,RC                                                            
*                                                                               
XIT      XIT1                                                                   
         SPACE 3                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TAREPQUESD                                                     
         EJECT                                                                  
       ++INCLUDE TAREPFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPC7D                                                       
         EJECT                                                                  
* DDGENTWA   (MUST FOLLOW LAST SCREEN)                                          
* DDPERVAL                                                                      
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* DDTWADCOND                                                                    
* TAGENFILE                                                                     
* TASYSDSECT                                                                    
* TASYSEQUS                                                                     
* TAREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDTWADCOND                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAREPWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'087TAREP47   05/27/03'                                      
         END                                                                    
