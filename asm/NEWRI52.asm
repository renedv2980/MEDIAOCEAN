*          DATA SET NEWRI52    AT LEVEL 036 AS OF 03/14/18                      
*          DATA SET NEWRI52    AT LEVEL 091 AS OF 08/10/98                      
*PHASE T32052A,+0                                                               
*INCLUDE CLUNPK                                                                 
*INCLUDE TIMEOUT                                                                
*INCLUDE DLFLD                                                                  
         TITLE 'T32052 - UPLOAD PACKAGE HEADER REPORT'                          
T32052   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**UPLD**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T320FFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS2          ANETWS2=WORKING STORAGE                      
         LA    R7,30(R7)           ANETWS1 HAS 255 PROD LIST                    
         USING WORKD,R7                                                         
         L     R6,ANETWS4                                                       
         USING NDDEMBLK,R6                                                      
*                                                                               
         L     RE,SYSPARMS                                                      
         L     R1,16(RE)           A(COMFACS)                                   
         USING COMFACSD,R1                                                      
         MVC   NDGLOBBR,CGLOBBER                                                
         DROP  R1                                                               
         SPACE 1                                                                
         CLI   MODE,PRINTREP                                                    
         BNE   RP2                                                              
         BAS   RE,REPMOD                                                        
         B     XIT                                                              
*                                                                               
RP2      CLI   MODE,VALREC                                                      
         BNE   RP4                                                              
         BAS   RE,EDITMOD                                                       
         CLI   OFFLINE,C'Y'                                                     
         BNE   XIT                                                              
         BRAS  RE,INITFAX                                                       
         B     XIT                                                              
RP4      EQU   *                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              EDIT ROUTINES                                                    
         SPACE                                                                  
EDITMOD  NTR1                                                                   
*                                                                               
         CLC   =C'EH',CONREC       IF EH REPORT SKIP CHECK                      
         BNE   NOTEH                                                            
         XC    CONOUT,CONOUT                                                    
         MVC   CONOUT(4),=C'DOWN'                                               
         MVI   CONOUTH+5,4                                                      
         OI    CONOUTH+6,X'80'                                                  
NOTEH    EQU   *                                                                
*                                                                               
         LA    R2,CONWHENH                                                      
         CLC   =C'DOWN',CONOUT                                                  
         BNE   EDIT00                                                           
         CLC   =C'NOW',CONWHEN                                                  
         BE    DOWNERR                                                          
         MVI   DOWNLOAD,C'Y'                                                    
         B     EDITM1                                                           
EDIT00   LA    R2,CONWHENH                                                      
         CLC   =C'SOON',CONWHEN                                                 
         BNE   EDITM1                                                           
         CLC   =C'EH',CONREC       IF EH REPORT SKIP CHECK                      
         BE    EDITM1                                                           
         CLC   =C'FX',CONDEST                                                   
         BE    EDITM1                                                           
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(34),=C'** ERROR -IF PRINT=SOON,DEST=FX **'               
         GOTO1 ERREX2                                                           
DOWNERR  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(34),=C'** ERROR -DOWN AND NOW INVALID  **'               
         GOTO1 ERREX2                                                           
*                                                                               
EDITM1   MVI   NBQINIT,0           DO ALL VALIDATIONS EACH TIME                 
         LA    RE,WAGYID           CLEAR WORK FIELDS                            
         LA    RF,WLENE                                                         
         XCEF                                                                   
*                                                                               
         L     R4,NBAIO                                                         
         USING CTIREC,R4                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKEY,C'I'                                                      
         MVC   CTIKNUM,T320FFD+10                                               
         GOTO1 DATAMGR,DMCB,(0,=CL8'DMREAD'),=CL8'CTFILE',(R4),(R4)             
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    RE,CTIDATA                                                       
CT10     CLI   0(RE),X'02'                                                      
         BE    CT20                                                             
         ZIC   RF,1(RE)                                                         
         AR    RE,RF                                                            
         CLI   0(RE),0                                                          
         BNE   CT10                                                             
         DC    H'0'                                                             
CT20     MVC   WAGYID,2(RE)                                                     
         MVC   WAGYNM,USERNAME                                                  
         DROP  R4                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
         MVI   FTERMFLG,0                FOLLOWING FIELDS ARE REQUIRED          
*                                                                               
         LA    R2,SPLCLIH                          CLIENT                       
         NETGO NVCLI,DMCB,WCLTNM,ANETWS1                                        
         L     R1,NBAIO                                                         
         USING CLTHDR,R1                                                        
         L     R2,ANETWS1                                                       
         LA    R3,220                                                           
CLST05   CLI   0(R2),0                                                          
         BE    NEWCLST                                                          
         LA    R2,4(R2)                                                         
         BCT   R3,CLST05                                                        
NEWCLST  MVC   0(140,R2),CLIST2                                                 
         DROP  R1                                                               
         MVC   WCLT,NBSELCLI                                                    
*                                                                               
         XC    NBSELPRD,NBSELPRD                    PRODUCT FUDGE               
*                                                                               
         LA    R2,SPLESTH                           ESTIMATE                    
         NETGO NVEST,DMCB,WESTNM,NDDEMBLK                                       
         MVC   WEST,SPLEST                                                      
         CLI   SPLDEMH+5,0                    ..DID USER INPUT DEMOS            
         BNE   UPL30                                                            
         BAS   RE,DEMRTN                      ..NO/TAKE OF EST RECORD           
*                                                                               
UPL30    L     R4,NBAIO                       ESTIMATE START/END DATES          
         USING ESTHDR,R4                                                        
         GOTO1 DATCON,DMCB,ESTART,(20,WESTRT)                                   
         GOTO1 DATCON,DMCB,EEND,(20,WEEND)                                      
         B     UPL32                                                            
         MVC   WESTRT+2(6),ESTART                                               
         MVC   WEEND+2(6),EEND                                                  
         MVC   WESTRT(2),=C'19'                                                 
         MVC   WEEND(2),=C'19'                                                  
         CLC   =C'50',ESTART       ASSUME ABOVE 50 ITS 1900'S                   
         BL    UPL32                                                            
         MVC   WESTRT(2),=C'20'    ITS THE FAMOUS YEAR 2000                     
         MVC   WEEND(2),=C'20'                                                  
UPL32    EQU   *                                                                
*                                                                               
         LA    R2,SPLNETH               NETWORK                                 
         NETGO NVNET,DMCB                                                       
         MVC   WNET,NBSELNET                                                    
         EJECT                                                                  
*                                                                               
         LA    R2,SPLPAKH               PACKAGE                                 
         BAS   RE,PAKRANGE         ARE WE DOING PACKAGE RANGE                   
         BE    UPL40               YES                                          
         NETGO NVPAK,DMCB          NO                                           
         CLC   NBKEY(20),NBKEYLST                                               
         BNE   EDINV                                                            
                                                                                
*- CHECK IF UNITS UNDER PACKAGE                                                 
***      L     R2,NBAIO                                                         
***      USING NPRECD,R2                                                        
***      TM    NPAKCNTL,X'20'      NO UNITS UNDER PACKAGE                       
***      BO    UPL38                                                            
***      XC    CONHEAD,CONHEAD                                                  
***      MVC   CONHEAD(36),=C'** ERROR - UNITS UNDER PACKAGE    **'             
***      LA    R3,CONHEAD+31                                                    
***      EDIT  (B1,NPKPACK),(3,0(R3))                                           
***      GOTO1 ERREX2                                                           
***                                                                             
*- CHECK IF PACKAGE CABLE LOCKED                                                
***UPL38    TM    NPAKCNTL,X'08'      IS PACKAGE CABLE LOCKED                   
***      BO    UPL39               YES, OK                                      
***      XC    CONHEAD,CONHEAD                                                  
***      MVC   CONHEAD(40),=C'* ERROR - PACKAGE     NOT CABLE LOCKED *'         
***      LA    R3,CONHEAD+18                                                    
***      EDIT  (B1,NPKPACK),(3,0(R3))                                           
***      GOTO1 ERREX2                                                           
                                                                                
UPL39    DS    0H                                                               
         MVC   WPAK,SPLPAK                                                      
UPL40    L     R2,NBAIO                                                         
         USING NPAKEL,R2                                                        
         MVC   NBDTADSP,=H'27'                                                  
         MVI   ELCODE,1                                                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R3,WPAKNM                                                        
         MVC   WPAKNM(16),NPAKNAME          PACKAGE NAME                        
*        EXPDP WPAKNM+17,NPAKDP             DAYPART AND                         
*        MVI   WPAKNM+26,C'$'               COST                                
*        EDIT  (4,NPAKCOST),(8,27(R3)),ALIGN=LEFT,ZERO=NOBLANK                  
*        GOTO1 SQUASHER,DMCB,WPAKNM,40                                          
         MVI   WVPHIMP,C'I'                                                     
         TM    NPAKCNTL,X'40'                IMP BASED                          
         BO    *+8                                                              
         MVI   WVPHIMP,C'V'                                                     
         CLI   NPAKMAST,0                                                       
         BE    SKIPRD                                                           
         MVC   BYTE,NPAKMAST                                                    
         BRAS  RE,GETPRD           SETS 3 CHAR PROD CODE INTO WMALLOC           
*                                                                               
SKIPRD   MVI   FTERMFLG,1                     OPTIONAL FIELDS                   
         LA    R2,SPLDEMH                     DEMOS                             
         CLI   5(R2),0                                                          
         BE    UPL70                                                            
         MVI   NDNDEMOS,20                     MAX 20 DEMOS                     
         NETGO NVDEM,DMCB,DBLOCK,NDDEMBLK                                       
         BAS   RE,DEMRTN                                                        
*                                                                               
UPL70    DS    0H                                                               
         LA    R2,SPLCOMH                      COMMENT                          
         OC    8(30,R2),8(R2)                                                   
         BZ    UPL90                                                            
         LA    R3,WCOMMENT                                                      
         LA    R4,6                                                             
UPL80    MVC   0(50,R3),8(R2)                                                   
         LA    R3,50(R3)                                                        
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         BCT   R4,UPL80                                                         
*                                                                               
UPL90    DS    0H                                                               
         LA    R2,SPLDELH                   DEAL NUMBER                         
         OC    8(10,R2),8(R2)                                                   
         BZ    UPL92                                                            
         LA    R3,WDEAL                                                         
         MVC   0(10,R3),8(R2)                                                   
*                                                                               
UPL92    DS    0H                                                               
         LA    R2,SPLCONH                   CONTRACT                            
         OC    8(10,R2),8(R2)                                                   
         BZ    EDTX                                                             
         LA    R3,WCONTRCT                                                      
         MVC   0(10,R3),8(R2)                                                   
*                                                                               
EDTX     LA    R2,SPLCLIH                                                       
*  MAKE SURE WE ARE COMING FROM STEWARD                                         
         CLC   SPLNAM(3),=CL3'EDI'                                              
         BNE   EDTXX                                                            
         CLC   CONREC+5(2),=CL2'ST'                                             
         BNE   EDTXX                                                            
         LA    RE,CONHEAD                                                       
         ST    RE,WORK                                                          
         GOTO1 NDGLOBBR,DMCB,=C'PUTD',WORK,4,GLVBUY2                            
*                                                                               
EDTXX    XIT1  REGS=(R2)                                                        
         EJECT                                                                  
* - HANDLES PACKAGE RANGE                                                       
PAKRANGE NTR1                                                                   
         XC    BLOCK(96),BLOCK                                                  
         GOTO1 SCANNER,DMCB,(R2),(3,BLOCK)        LOOK FOR RANGE                
         MVI   PAKRGS,0                           PRESET TO NO RANGE            
         MVI   PAKRGE,0                                                         
         CLI   DMCB+4,2             MUST BE 2 PARTS FOR RANGE                   
         BH    EDINV                                                            
         BL    PKRGNO                                                           
         MVC   PAKRGS,BLOCK+7       FIRST RANGE VALUE                           
         CLI   PAKRGS,0             ZERO MEANS NON-NUMERIC OR ZERO              
         BE    EDINV                                                            
         MVC   PAKRGE,BLOCK+39      SECOND RANGE VALUE                          
         CLC   PAKRGE,PAKRGS        CK FOR 0 OR LESS THAN 1ST VALUE             
         BNH   EDINV                                                            
* - GET 1ST PACKAGE RECORD OF RANGE INTO NBAIO                                  
         LA    R2,KEY                                                           
         USING NPRECD,R2                                                        
         XC    KEY,KEY                                                          
         MVI   NPKTYPE,X'02'                                                    
         MVC   NPKAM(3),NBACTAM                                                 
         MVC   NPKNET,NBSELNET                                                  
         MVC   NPKEST,NBSELEST                                                  
         MVC   NPKPACK,PAKRGS                                                   
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'UNTDIR  ',KEY,KEY,0                   
         CLC   KEY(20),KEYSAVE                                                  
         BNE   PKGERR                                                           
         MVC   NBKEY,KEY           GETPRD EXPECTS THIS                          
         L     R2,NBAIO                                                         
         LA    R3,KEY+21                                                        
         LA    R4,DMWORK                                                        
         GOTO1 DATAMGR,DMCB,(0,=C'GETREC'),=C'UNTFILE ',(R3),(R2),(R4)          
         EDIT  (B1,PAKRGS),(3,WPAK),ALIGN=LEFT     PKG NUMBER TO DSECT          
         BAS   RE,CHKPAK                                                        
PKRGYES  SR    RE,RE                                                            
PKRGNO   LTR   RE,RE                                                            
PKRX     B     XIT                                                              
                                                                                
CHKPAK   NTR1                                                                   
***      BAS   RE,CHKUNITS                                                      
CHP10    MVC   WORK(20),KEY                                                     
         GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'UNTDIR  ',KEY,KEY,0                   
         CLC   WORK(19),KEY                                                     
         BNE   CHPX                                                             
         CLC   KEY+19(1),PAKRGE    IS IT WITHIN PACKAGE RANGE                   
         BH    CHPX                                                             
         L     R2,NBAIO                                                         
         LA    R3,KEY+21                                                        
         LA    R4,DMWORK                                                        
         GOTO1 DATAMGR,DMCB,(0,=C'GETREC'),=C'UNTFILE ',(R3),(R2),(R4)          
***      BAS   RE,CHKUNITS         CHK IF UNITS UNDER PKG                       
         B     CHP10               AND GET NEXT PACKAGE                         
CHPX     B     XIT                                                              
                                                                                
*                                                                               
CHKUNITS NTR1                                                                   
*- CHECK IF UNITS UNDER PACKAGE                                                 
         USING NPRECD,R2                                                        
         TM    NPAKCNTL,X'20'      NO UNITS UNDER PACKAGE                       
         BO    CHKX                                                             
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(36),=C'** ERROR - UNITS UNDER PACKAGE    **'             
         LA    R3,CONHEAD+31                                                    
         EDIT  (B1,NPKPACK),(3,0(R3))                                           
         GOTO1 ERREX2                                                           
CHKX     B     XIT                                                              
*                                                                               
EDINV    DS    0H                                                               
         MVI   ERROR,INVALID                                                    
         GOTO1 ERREX                                                            
*                                                                               
PKGERR   DS    0H                                                               
         LA    R2,SPLPAKH                                                       
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(21),=C'*** PACKAGE ERROR ***'                            
         GOTO1 ERREX2                                                           
*                                                                               
         EJECT                                                                  
*                                                                               
DEMRTN   NTR1                                                                   
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'NTI'                                                   
         MVI   DBSELMED,C'N'                                                    
         MVI   DBSELSRC,C'N'                                                    
         LA    R2,1                                                             
         LA    R3,NDDEMBLK         GET THE DEMOS,SKIP NAD,SKIP HOMES            
         LA    R4,WDEMOS                                                        
         SR    R5,R5                                                            
         XC    WORK(10),WORK                                                    
DEM05    CLI   1(R3),X'21'         IS IT USER DEMO                              
         BE    DEM10                                                            
**       CLI   0(R3),0             .IS IT NAD DEMO  PER BARTB NOV695            
**       BNE   DEM20               .YES/SKIP IT                                 
         CLI   2(R3),1             ...IS IT HOMES                               
         BE    DEM20               ...YES/SKIP IT                               
DEM10    NETGO NVDEMCON,DMCB,((R5),NDDEMBLK),DBLOCK,(10,WORK)                   
*                                                                               
         LA    R1,WDEMOS           ADD TO TABLE/SKIP DUPES                      
DEM12    CLI   0(R1),0                                                          
         BE    DEM15                                                            
         CLC   WORK+1(10),0(R1)     ..IS IT A DUPE                              
         BE    DEM20               ..SKIP THAT TOO                              
         LA    R1,11(R1)           DEMO=10 MAX PLUS BLANK DIVIDER               
         B     DEM12                                                            
DEM15    CLI   WORK,C'R'           SKIP MODIFIERS                               
         BE    DEM17                                                            
         CLI   WORK,C'*'                                                        
         BE    DEM20                                                            
         CLI   WORK,C'I'                                                        
         BNE   *+10                                                             
DEM17    MVC   WORK(10),WORK+1                                                  
         MVC   0(10,R1),WORK                                                    
         OC    0(11,R1),SPACES     SET SPACE BETWEEN DEMOS                      
*                                  SINCE ROUTINE LOOKS FOR 0 TO ADD             
*                                  NEW DEMO NAME                                
DEM20    LA    R5,1(R5)                                                         
         LA    R3,3(R3)                                                         
         CLC   0(3,R3),SPACES                                                   
         BE    DEMX                                                             
         OC    0(3,R3),0(R3)                                                    
         BZ    DEMX                                                             
         CLI   0(R3),X'FF'                                                      
         BE    DEMX                                                             
         LA    R2,1(R2)                                                         
         C     R2,=F'20'                                                        
         BH    DEMX                                                             
         B     DEM05                                                            
DEMX     B     XIT                                                              
*                                                                               
         EJECT                                                                  
*                                                                               
REPMOD   NTR1                                                                   
         CLI   DOWNLOAD,C'Y'                                                    
         BE    RP05                                                             
         LA    R1,MYSPECS                                                       
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
RP05     DS    0H                                                               
         CLI   DOWNLOAD,C'Y'                                                    
         BE    RP45                                                             
         MVC   P+5(6),=C'AGENCY'                                                
         MVC   P+25(10),WAGYID      4 CHAR AGY SIGN-ON                          
         MVC   P+45(33),WAGYNM                                                  
         BAS   RE,SPOOLIT                                                       
*                                                                               
         MVC   P+5(6),=C'MEDIA '                                                
         MVI   P+25,C'C'                                                        
         BAS   RE,SPOOLIT                                                       
*                                                                               
         MVC   P+5(6),=C'CLIENT'                                                
         MVC   P+25(3),WCLT        CLIENT CODE                                  
         MVC   P+45(20),WCLTNM                                                  
         BAS   RE,SPOOLIT                                                       
*                                                                               
         MVC   P+5(7),=C'NETWORK'                                               
         MVC   P+25(4),WNET                                                     
         BAS   RE,SPOOLIT                                                       
*                                                                               
         MVC   P+5(8),=C'ESTIMATE'                                              
         MVC   P+25(3),WEST                                                     
         MVC   P+45(20),WESTNM                                                  
         BAS   RE,SPOOLIT                                                       
*                                                                               
         MVC   P+5(10),=C'EST. START'                                           
         MVC   P+25(8),WESTRT                                                   
         BAS   RE,SPOOLIT                                                       
         MVC   P+5(10),=C'EST. END  '                                           
         MVC   P+25(8),WEEND                                                    
         BAS   RE,SPOOLIT                                                       
*                                                                               
         MVC   P+5(7),=C'PACKAGE'                                               
         MVC   P+25(3),WPAK                                                     
         MVC   P+45(16),WPAKNM                                                  
         BAS   RE,SPOOLIT                                                       
*                                                                               
         MVC   P+5(12),=C'VPH/IMP BASE'                                         
         MVC   P+25(1),WVPHIMP                                                  
         BAS   RE,SPOOLIT                                                       
*                                                                               
         EJECT                                                                  
*                                                                               
         MVC   P+5(15),=C'DEMO CATEGORIES'                                      
         LA    R3,WDEMOS                                                        
         LA    R2,P+25                                                          
         LA    R4,40               MAX DEMO NAME CHARACTERS PER LINE            
RP10     MVC   0(1,R2),0(R3)                                                    
         LA    R3,1(R3)                                                         
         CLI   0(R3),X'40'         ,,ANY MORE DEMO NAME CHARACTERS?             
         BH    RP15                ,,YES                                        
         LA    R2,1(R2)                  (SPACE BETWEEN NAMES)                  
                                                                                
         LA    R0,8                ,,NO/ANY MORE DEMO NAMES?                    
RP12     CLI   0(R3),X'40'                                                      
         BNH   RP14                                                             
         C     R4,=F'11'           NEED AT LEAST 11 SPACES                      
         BNL   RP15                GOT EM/GO ON                                 
         C     R4,=F'7'            HOW ABOUT 7 SPACES?                          
         BL    RP16                NOPE/NEED NEW PRINT LINE                     
                                                                                
         LA    R0,7                HAVE ROOM FOR 6 CHAR DEMO/DEMO=6?            
         LR    R1,R3               POINTS TO START OF DEMO NAME                 
RP13     CLI   0(R1),X'40'                                                      
         BNH   RP15                YES/6 CHAR ACTER DEMO/GO ON                  
         LA    R1,1(R1)            BUMP DEMO NAME                               
         BCT   R0,RP13                                                          
         B     RP16                GREATER THAN 6 CHAR NAME/NEW P LINE          
                                                                                
RP14     LA    R3,1(R3)                                                         
         BCT   R0,RP12                                                          
         B     RP30                NO MORE DEMOS                                
                                                                                
                                                                                
RP15     LA    R2,1(R2)            BUMP PRINT LINE                              
         BCT   R4,RP10                                                          
RP16     BAS   RE,SPOOLIT          PRINT IT                                     
         LA    R2,P+25             SET NEW PRINT LINE                           
         LA    R4,40                                                            
         B     RP10                                                             
RP30     BAS   RE,SPOOLIT                                                       
*                                                                               
         MVC   P+5(12),=C'MASTER ALLOC'                                         
         MVC   P+25(3),WMALLOC                                                  
         MVC   P+45(20),WPRODNAM                                                
         BAS   RE,SPOOLIT                                                       
         BAS   RE,SPOOLIT                                                       
*                                                                               
         MVC   P+5(9),=C'COMMENTS:'                                             
         LA    R2,WCOMMENT                                                      
         LA    R3,P+18                                                          
RP35     OC    0(20,R2),0(R2)                                                   
         BZ    RP37                                                             
         MVC   0(50,R3),0(R2)                                                   
         BAS   RE,SPOOLIT                                                       
         LA    R2,50(R2)                                                        
         B     RP35                                                             
*                                                                               
RP37     BAS   RE,SPOOLIT                                                       
         MVC   P+5(6),=C'DEAL #'                                                
         LA    R2,WDEAL                                                         
         LA    R3,P+18                                                          
         OC    0(10,R2),0(R2)                                                   
         BZ    RP39                                                             
         MVC   0(10,R3),0(R2)                                                   
         BAS   RE,SPOOLIT                                                       
         BAS   RE,SPOOLIT                                                       
RP39     MVC   P+5(10),=C'CONTRACT #'                                           
         LA    R2,WCONTRCT                                                      
         LA    R3,P+18                                                          
         OC    0(10,R2),0(R2)                                                   
         BZ    RP50                                                             
         MVC   0(10,R3),0(R2)                                                   
         BAS   RE,SPOOLIT                                                       
         B     RP50                                                             
*                                                                               
RP45     GOTO1 =A(DOWNLD),DMCB,(RC)                                             
*                                                                               
*                                                                               
RP50     DS    0H                                                               
         CLI   PAKRGS,0            ARE WE DOING PACKAGE RANGE                   
         BE    RPX                 NO                                           
         CLC   PAKRGS,PAKRGE       YES/ARE WE FINISHED                          
         BE    RPX                     YES                                      
         CLC   PAKRGS,PAKRGE           NO/SAFETY CHECK                          
         BH    RPX                                                              
         ZIC   R1,PAKRGS           GET NEXT PKG NUMBER                          
         LA    R1,1(R1)                                                         
         STC   R1,PAKRGS                                                        
* - GET PACKAGE RECORD INTO NBAIO                                               
         NETGO NVSETUNT,DMCB                                                    
         LA    R2,KEY                                                           
         USING NPRECD,R2                                                        
         XC    KEY,KEY                                                          
         MVI   NPKTYPE,X'02'                                                    
         MVC   NPKAM(3),NBACTAM                                                 
         MVC   NPKNET,NBSELNET                                                  
         MVC   NPKEST,NBSELEST                                                  
         MVC   NPKPACK,PAKRGS                                                   
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'UNTDIR  ',KEY,KEY,0                   
         CLC   KEY(20),KEYSAVE                                                  
         BNE   RP50                                                             
         MVC   NBKEY,KEY           SET FOR GETPRD ROUTINE                       
         LA    R3,KEY+21                                                        
         L     R2,NBAIO                                                         
         LA    R4,DMWORK                                                        
         GOTO1 DATAMGR,DMCB,(0,=C'GETREC'),=C'UNTFILE ',(R3),(R2),(R4)          
         EDIT  (B1,PAKRGS),(3,WPAK),ALIGN=LEFT     PKG NUMBER TO DSECT          
         L     R2,NBAIO                                                         
         USING NPAKEL,R2                                                        
         MVI   ELCODE,1                                                         
         MVC   NBDTADSP,=H'27'                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R3,WPAKNM                                                        
         XC    WPAKNM,WPAKNM                                                    
         MVC   WPAKNM(16),NPAKNAME            PACKAGE NAME                      
*        EXPDP WPAKNM+17,NPAKDP                                                 
*        MVI   WPAKNM+26,C'$'                                                   
*        EDIT  (4,NPAKCOST),(8,27(R3)),ALIGN=LEFT,ZERO=NOBLANK                  
*        GOTO1 SQUASHER,DMCB,WPAKNM,40                                          
         MVI   WVPHIMP,C'I'                                                     
         TM    NPAKCNTL,X'40'                 IMP BASED                         
         BO    *+8                                                              
         MVI   WVPHIMP,C'V'                                                     
         CLI   NPAKMAST,0                                                       
         BE    RP70                                                             
         MVC   BYTE,NPAKMAST                                                    
         BRAS  RE,GETPRD           SETS 3 CHAR PROD CODE INTO WMALLOC           
RP70     MVI   FORCEHED,C'Y'                                                    
         B     RP05                                                             
RPX      CLI   DOWNLOAD,C'Y'                                                    
         BNE   RPXX                                                             
         MVI   DOWNLOAD,C'E'                                                    
         GOTO1 =A(DOWNLD),DMCB,(RC)                                             
RPXX     B     XIT                                                              
         DROP  R2                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
SPOOLIT  NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
*                                                                               
         SPACE 2                                                                
*                                                                               
         GETEL (R2),NBDTADSP,ELCODE                                             
*                                                                               
         EJECT                                                                  
MYSPECS  DS    0F                                                               
         SSPEC H1,1,C'MEDIA     NETWORK T.V.'                                   
         SSPEC H1,50,C'PACKAGE HEADER REPORT'                                   
         SSPEC H2,50,C'---------------------'                                   
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,99,AGYNAME                                                    
         SSPEC H2,99,AGYADD                                                     
         SSPEC H4,99,RUN                                                        
         DC    X'00'                                                            
         SPACE                                                                  
*                                                                               
HOOK     NTR1                                                                   
         MVC   H8+5(5),=C'FIELD'                                                
         MVC   H9+5(5),=C'-----'                                                
         MVC   H8+25(8),=C'AGY CODE'                                            
         MVC   H9+25(8),=C'--------'                                            
         MVC   H8+45(11),=C'AGENCY NAME'                                        
         MVC   H9+45(11),=C'-----------'                                        
         XC    DMCB(16),DMCB                                                    
**       LA    R4,RELO                                                          
**       S     R4,RELO                                                          
**       GOTO1 =V(TICTOC),DMCB,C'SGET',0,RR=R4                                  
**       MVC   FULL,0(R1)                                                       
*                                                                               
         THMS  DDSTIME=YES                                                      
         ST    R0,DUB              ACTUAL TIME ADJUSTMENT                       
         ST    R1,DUB+4            DDS TIME                                     
         AP    DUB(4),DUB+4(4)                                                  
         GOTO1 =V(TIMEOUT),DMCB,(X'03',DUB),(X'81',WORK),RR=YES                 
         CLI   WORK+6,X'F0'        DROP LEADING ZERO                            
         BNE   *+8                                                              
         MVI   WORK+6,C' '                                                      
         MVC   H4+117(2),WORK+6    HH                                           
         MVI   H4+119,C':'         SET MARK                                     
         MVC   H4+120(4),WORK+8    MM (+AM/PM)                                  
         B     XIT                                                              
*                                                                               
RELO     DC    A(*)                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
DOWNLD   NMOD1 0,**N52D                                                         
         L     RC,0(R1)                                                         
         LA    R4,DOWNWORK                                                      
         USING DLCBD,R4                                                         
         LA    R2,P                                                             
         ST    R2,DLCBAPL                                                       
         LA    R2,DOWNHK                                                        
         ST    R2,DLCBAPR                                                       
         CLI   DOWNLOAD,C'E'       END DOWNLOAD?                                
         BNE   DL00                                                             
         MVI   DLCBACT,C'R'        END OF REPORT                                
         BAS   RE,VDLFLD                                                        
         B     DOWNLDX                                                          
DL00     MVI   DLCBACT,C'S'         START                                       
         MVI   DLCBFLD,X'40'                                                    
         MVC   DLCBFLD+1(L'DLCBFLD-1),DLCBFLD                                   
         BAS   RE,VDLFLD                                                        
*                                                                               
         LA    R3,DLCBFLD                                                       
         MVI   DLCBACT,C'P'                                                     
         MVI   DLCBTYP,C'T'                                                     
         MVC   0(8,R3),WAGYID      8 CHAR AGY SIGN-ON                           
         BAS   RE,VDLFLD                                                        
*                                                                               
         MVI   DLCBACT,C'P'                                                     
         MVI   DLCBTYP,C'T'                                                     
         MVC   0(32,R3),WAGYNM                                                  
         BAS   RE,VDLFLD                                                        
         MVI   DLCBACT,C'L'                                                     
         BAS   RE,VDLFLD                                                        
*                                                                               
         MVI   DLCBACT,C'P'                                                     
         MVI   DLCBTYP,C'T'                                                     
         MVI   0(R3),C'C'          MEDIA                                        
         BAS   RE,VDLFLD                                                        
*                                                                               
         MVI   DLCBACT,C'P'                                                     
         MVI   DLCBTYP,C'T'                                                     
         MVI   0(R3),X'40'         MEDIA NAME                                   
         BAS   RE,VDLFLD                                                        
         MVI   DLCBACT,C'L'                                                     
         BAS   RE,VDLFLD                                                        
*                                                                               
         MVI   DLCBACT,C'P'                                                     
         MVI   DLCBTYP,C'T'                                                     
         MVC   0(3,R3),WCLT        CLIENT                                       
         BAS   RE,VDLFLD                                                        
*                                                                               
         MVI   DLCBACT,C'P'                                                     
         MVI   DLCBTYP,C'T'                                                     
         MVC   0(20,R3),WCLTNM                                                  
         BAS   RE,VDLFLD                                                        
         MVI   DLCBACT,C'L'                                                     
         BAS   RE,VDLFLD                                                        
*                                                                               
         MVI   DLCBACT,C'P'                                                     
         MVI   DLCBTYP,C'T'                                                     
         MVC   0(4,R3),WNET        NETWORK                                      
         BAS   RE,VDLFLD                                                        
*                                                                               
         MVI   DLCBACT,C'P'                                                     
         MVI   DLCBTYP,C'T'                                                     
         MVI   0(R3),X'40'       NETWORK NAME (FOR FUTURE USE)                  
         BAS   RE,VDLFLD                                                        
         MVI   DLCBACT,C'L'                                                     
         BAS   RE,VDLFLD                                                        
*                                                                               
         MVI   DLCBACT,C'P'                                                     
         MVI   DLCBTYP,C'T'                                                     
         MVC   0(3,R3),WEST        ESTIMATE                                     
         BAS   RE,VDLFLD                                                        
         MVI   DLCBACT,C'P'                                                     
         MVI   DLCBTYP,C'T'                                                     
         MVC   0(20,R3),WESTNM                                                  
         BAS   RE,VDLFLD                                                        
         MVI   DLCBACT,C'L'                                                     
         BAS   RE,VDLFLD                                                        
*                                                                               
         MVI   DLCBACT,C'P'                                                     
         MVI   DLCBTYP,C'T'                                                     
         MVC   0(8,R3),WESTRT                                                   
         BAS   RE,VDLFLD                                                        
         MVI   DLCBACT,C'P'                                                     
         MVI   DLCBTYP,C'T'                                                     
         MVC   0(8,R3),WEEND                                                    
         BAS   RE,VDLFLD                                                        
         MVI   DLCBACT,C'L'                                                     
         BAS   RE,VDLFLD                                                        
*                                                                               
         MVI   DLCBACT,C'P'                                                     
         MVI   DLCBTYP,C'T'                                                     
         MVC   0(3,R3),WPAK                                                     
         BAS   RE,VDLFLD                                                        
*                                                                               
         MVI   DLCBACT,C'P'                                                     
         MVI   DLCBTYP,C'T'                                                     
         MVC   0(16,R3),WPAKNM                                                  
         BAS   RE,VDLFLD                                                        
         MVI   DLCBACT,C'L'                                                     
         BAS   RE,VDLFLD                                                        
*                                                                               
         MVI   DLCBACT,C'P'                                                     
         MVI   DLCBTYP,C'T'                                                     
         MVC   0(1,R3),WVPHIMP                                                  
         BAS   RE,VDLFLD                                                        
         MVI   DLCBACT,C'L'                                                     
         BAS   RE,VDLFLD                                                        
*                                                                               
         LA    R2,WDEMOS                                                        
         LA    R5,WMALLOC                                                       
         SR    RE,RE                                                            
         LA    RF,5                                                             
DL50     MVC   0(1,R3),0(R2)                                                    
         LA    R2,1(R2)            BUMP WDEMOS                                  
         LA    RE,1(RE)            BUMP LEN COUNTER                             
         CR    R2,R5               END OF DEMO STORAGE?                         
         BNL   DL55                YES                                          
         LA    R3,1(R3)            NO-BUMP OUT AREA                             
         CLI   0(R2),X'40'         ANY MORE DEMO NAME CHARACTERS?               
         BH    DL50                YES                                          
*                                                                               
         BCT   RF,DL51             PASS A MAX OF 5 DEMOS                        
         MVI   DLCBACT,C'P'        PRINT THEM                                   
         MVI   DLCBTYP,C'T'                                                     
         STC   RE,DLCBLEN                                                       
         BAS   RE,VDLFLD                                                        
         SR    RE,RE               RESTORE RE AS COUNTER                        
         LA    R3,DLCBFLD          RESTORE R3                                   
         LA    RF,5                RESTORE BCT DEMO CATEGORY COUNTER            
DL50A    LA    R2,1(R2)            ANY MORE CATEGORIES?                         
         CR    R2,R5                                                            
         BNL   DL56                NO-CLOSE LINE                                
         CLI   0(R2),X'40'                                                      
         BH    DL50                YES-GET THEM                                 
         B     DL50A               CONTINUE LOOKING                             
*                                                                               
DL51     LA    R3,1(R3)            SPACE BETWEEN CATEGORIES                     
         LA    RE,1(RE)                                                         
DL52     LA    R2,1(R2)            BUMP TO NEXT DEMO CATEGORY                   
         CR    R2,R5               END OF DEMO STORAGE?                         
         BNL   DL55                                                             
         CLI   0(R2),X'40'         HAVE WE HIT A CATEGORY?                      
         BH    DL50                                                             
         B     DL52                                                             
DL55     MVI   DLCBACT,C'P'                                                     
         MVI   DLCBTYP,C'T'                                                     
         STC   RE,DLCBLEN                                                       
         BAS   RE,VDLFLD                                                        
DL56     MVI   DLCBACT,C'L'                                                     
         BAS   RE,VDLFLD                                                        
                                                                                
*                                                                               
         MVI   DLCBACT,C'P'                                                     
         MVI   DLCBTYP,C'T'                                                     
         MVI   0(R3),X'40'                                                      
         OC    WMALLOC,WMALLOC                                                  
         BZ    DL57                                                             
         MVC   0(3,R3),WMALLOC                                                  
DL57     BAS   RE,VDLFLD                                                        
*                                                                               
         MVI   DLCBACT,C'P'                                                     
         MVI   DLCBTYP,C'T'                                                     
         MVI   0(R3),X'40'                                                      
         OC    WPRODNAM,WPRODNAM                                                
         BZ    DL58                                                             
         MVC   0(20,R3),WPRODNAM                                                
         OC    0(20,R3),SPACES                                                  
DL58     BAS   RE,VDLFLD                                                        
         MVI   DLCBACT,C'L'        END OF LINE                                  
         BAS   RE,VDLFLD                                                        
                                                                                
         LA    R2,WCOMMENT                                                      
         OC    0(20,R2),0(R2)                                                   
         BNZ   DL60                                                             
         MVI   0(R3),X'40'                                                      
         MVI   DLCBACT,C'P'                                                     
         MVI   DLCBTYP,C'T'                                                     
         BAS   RE,VDLFLD                                                        
         MVI   DLCBACT,C'L'                                                     
         BAS   RE,VDLFLD                                                        
         B     DL63                                                             
DL60     MVI   P,C'"'              PUT THEM OUT MYSELF                          
         MVC   P+1(50),0(R2)                                                    
         MVI   P+51,C'"'                                                        
         LA    R2,50(R2)                                                        
         OC    0(50,R2),0(R2)                                                   
         BZ    DL61                                                             
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     DL60                                                             
DL61     MVI   P+53,X'5E'        SEMICOLON!                                     
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
DL63     OC    WDEAL,WDEAL                                                      
         BNZ   DL63B                                                            
         MVI   0(R3),X'40'                                                      
         MVI   DLCBACT,C'P'                                                     
         MVI   DLCBTYP,C'T'                                                     
         BAS   RE,VDLFLD                                                        
         MVI   DLCBACT,C'L'                                                     
         BAS   RE,VDLFLD                                                        
         B     DL64                                                             
DL63B    MVI   DLCBACT,C'P'                                                     
         MVI   DLCBTYP,C'T'                                                     
         LA    R3,DLCBFLD                                                       
         MVC   0(10,R3),WDEAL                                                   
         BAS   RE,VDLFLD                                                        
         MVI   DLCBACT,C'L'                                                     
         BAS   RE,VDLFLD                                                        
DL64     OC    WCONTRCT,WCONTRCT                                                
         BNZ   DL65                                                             
         MVI   0(R3),X'40'                                                      
         MVI   DLCBACT,C'P'                                                     
         MVI   DLCBTYP,C'T'                                                     
         BAS   RE,VDLFLD                                                        
         MVI   DLCBACT,C'L'                                                     
         BAS   RE,VDLFLD                                                        
         B     DOWNLDX                                                          
DL65     MVI   DLCBACT,C'P'                                                     
         MVI   DLCBTYP,C'T'                                                     
         MVC   0(10,R3),WCONTRCT    CONTRACT                                    
         BAS   RE,VDLFLD                                                        
         MVI   DLCBACT,C'L'                                                     
         BAS   RE,VDLFLD                                                        
*                                                                               
DOWNLDX  XIT1                                                                   
*                                                                               
VDLFLD   NTR1                                                                   
         LR    R1,R4                                                            
         L     RF,=V(DLFLD)                                                     
         BASR  RE,RF                                                            
         XIT1                                                                   
*                                                                               
DOWNHK   NTR1                                                                   
         MVI   LINE,1                                                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
DOWNWORK DS    CL(DLCBXLX)                                                      
         EJECT                                                                  
*              INITIALIZE FAX DATA                                              
         SPACE 1                                                                
INITFAX  NTR1  BASE=*,LABEL=*                                                   
         L     R5,TWADCONS                                                      
         USING TWADCOND,R5                                                      
         L     R5,TFAXINFO                                                      
         USING FAXINFOD,R5                                                      
         MVC   FXITNMED,WMEDIA                                                  
         MVC   FXITNCLI,WCLT                                                    
         MVC   FXITNEST,WEST                                                    
         MVC   FXITNNET,WNET                                                    
         MVC   FXITNPAK,WPAK                                                    
         MVC   FXITNPRG,=C'PH'                                                  
         XIT1  XIT                                                              
         LTORG                                                                  
*                                                                               
                                                                                
*  BYTE HAS 1 BYTE PRD CODE / ANETWS1 HAS CLIST                                 
GETPRD   NTR1  BASE=*,LABEL=*                                                   
         L     R2,ANETWS1                                                       
GP10     CLI   0(R2),0             EOF                                          
         BE    GP20                                                             
         CLC   BYTE,3(R2)                                                       
         BE    GP15                                                             
         LA    R2,4(R2)                                                         
         B     GP10                                                             
GP15     MVC   WMALLOC,0(R2)                                                    
         LA    R1,NBKEY                                                         
         USING NPRECD,R1                                                        
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),NPKAM                                                   
         MVC   KEY+2(2),NPKCLT                                                  
         MVC   KEY+4(3),WMALLOC                                                 
         NETGO NVSETSPT,DMCB                                                    
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 GETREC                                                           
         L     R2,NBAIO                                                         
         USING PKEY,R2                                                          
         MVC   WPRODNAM,PNAME                                                   
         XC    FILENAME,FILENAME                                                
         NETGO NVSETUNT,DMCB                                                    
         B     GPX                                                              
GP20     MVC   WMALLOC,=C'UNA'                                                  
*                                                                               
GPX      XIT1                                                                   
         LTORG                                                                  
*                                                                               
WORKD    DSECT                                                                  
         DS    0D                                                               
WAGYID   DS    CL10                AGY ID (PASS ONLY 1ST 4 CHAR)                
WAGYNM   DS    CL33                AGY NAME                                     
WMEDIA   DS    CL1                 MEDIA                                        
WCLT     DS    CL3                 CLIENT                                       
WCLTNM   DS    CL40                CLIENT NAME                                  
WNET     DS    CL4                 NETWORK                                      
WEST     DS    CL3                 ESTIMATE                                     
WESTNM   DS    CL40                ESTIMATE NAME                                
WESTRT   DS    CL8                 EST START DATE                               
WEEND    DS    CL8                 EST END DATE                                 
WPAK     DS    CL3                 PACKAGE                                      
WPAKNM   DS    CL40                PACKAGE NAME                                 
WVPHIMP  DS    CL1                 VPH/IMP BASE                                 
*DEMOS   DS    CL210               DEMOS(NO NAD,NO HOMES,NO MODIFIER)           
WDEMOS   DS    CL230               DEMOS(NO NAD,NO HOMES,NO MODIFIER)           
WMALLOC  DS    CL3                 MASTER ALLOCATION                            
WPRODNAM DS    CL20                MASTER ALLOCATION PRODUCT NAME               
WCOMMENT DS    CL1000              COMMENTS                                     
WDEAL    DS    CL10                DEAL                                         
WCONTRCT DS    CL10                CONTRACT                                     
WLENE    EQU   *-WAGYID                                                         
PAKRGS   DS    CL1                 PACKAGE RANGE START                          
PAKRGE   DS    CL1                 PACKAGE RANGE END                            
DOWNLOAD DS    CL1                 PACKAGE RANGE END                            
NDGLOBBR DS    A                   GLOBBER ADDRESS                              
*                                                                               
TIMEWORK DS    6F                                                               
*                                                                               
*                                                                               
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPGENPROG                                                      
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE NEGENPACK                                                      
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE NETINCLS                                                       
       ++INCLUDE NETDEMOT                                                       
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDGLOBEQUS                                                     
         EJECT                                                                  
       ++INCLUDE NEWRIFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRID8D                                                       
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE DDTWADCOND                                                     
         EJECT                                                                  
       ++INCLUDE DDFAXINFOD                                                     
       ++INCLUDE DDDLCB                                                         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'036NEWRI52   03/14/18'                                      
         END                                                                    
