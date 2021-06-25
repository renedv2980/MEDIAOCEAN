*          DATA SET NEMEDCC    AT LEVEL 113 AS OF 03/03/09                      
*PHASE T31ECCA,+0                                                               
         TITLE 'T31ECC - DELETE LOCKED PACKAGES'                                
         PRINT   GEN                                                            
************************************************************                    
* DELETE LOCKED PACKAGES                                   *                    
*                                                          *                    
* THIS UPDATED VERSION OF THE DELPAK ALSO READS THROUGH    *                    
* UNLOCKED UNITS TO MAKE SURE THEY ARE NOT UNDER A LOCKED  *                    
* PACKAGE (SEEMS TO HAPPEN NOW AND THEN). SO CAN'T RELY    *                    
* ON NBSELPST TO HAVE NETIO ONLY PASS LOCKED PKGS/UNITS    *                    
************************************************************                    
T31ECC   CSECT                                                                  
         NMOD1 0,**DELP**,RR=R2                                                 
         PRINT NOGEN                                                            
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T31EFFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         LA    R1,HDHOOK                                                        
         ST    R1,HEADHOOK                                                      
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         L     R7,ANETWS4          ANETWS4=WORKING STORAGE                      
         USING WORKD,R7                                                         
         L     R1,ANETWS3          ANETWS3=CLIST                                
         ST    R1,ACLISTSV                                                      
         ST    R2,RELO                                                          
* - NEED TO HAVE COPIES WRITTEN TO RECOVERY FILE                                
         ICM   R1,15,TWAMASTC                                                   
         BZ    ENDMSTC                                                          
         USING MCBLOCK,R1                                                       
         L     R1,MCSSB                                                         
         USING SSBD,R1                                                          
         OI    SSBSTAT2,SSBSROLC   RECOVER OFFLINE COPIES                       
         DROP  R1                                                               
ENDMSTC  DS    0H                                                               
*                                                                               
         MVI   FRSTLAST,C'Y'                                                    
         CLI   MODE,VALKEY                                                      
         BE    VK                                                               
         CLI   MODE,PRINTREP                                                    
         BE    PR                                                               
         CLI   MODE,RUNFRST                                                     
         BE    FIRSTR                                                           
         CLI   MODE,RUNLAST                                                     
         BE    RUNLST                                                           
EXIT     XIT1                                                                   
                                                                                
*        PREPARE TOTAL FIELDS/ SET RUNLAST HOOK                                 
FIRSTR   L     R2,BOOKVAL                                                       
         ZAP   0(8,R2),=P'0'                                                    
         ZAP   8(8,R2),=P'0'                                                    
         L     R1,ATWA                                                          
         MVI   29(R1),2            SET TWAFIRST FOR RUNLAST HOOK                
         B     EXIT                                                             
*                                                                               
RUNLST   DS    0H                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   P(22),=C'*** ALL AGENCY RUN ***'                                 
         L     R2,BOOKVAL                                                       
         EDIT  (P8,0(R2)),(15,P+25),2                                           
         EDIT  (P8,8(R2)),(15,P+39),2                                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
* VALIDATE REQUEST SCREEN DATA                                                  
*                                                                               
*                                                                               
VK       DS    0H                                                               
         MVI   FTERMFLG,0          SET REQUIRED FLAG                            
*                                                                               
         LA    R2,SPLCLIH              CLIENT                                   
         NETGO NVCLI,DMCB,SPLCLIN                                               
         OI    SPLCLINH+6,X'80'                                                 
*                                                                               
         LA    R2,SPLESTH                ESTIMATE                               
         NETGO NVEST,DMCB,SPLESTN                                               
         OI    SPLESTNH+6,X'80'                                                 
*                                                                               
         LA    R2,SPLNETH                                                       
***      NETGO NVNETALL,DMCB,SPLNETN                                            
         NETGO NVNET,DMCB,SPLNETN                                               
                                                                                
         MVI   FTERMFLG,1          SET OPTIONAL FLAG                            
         LA    R2,SPLPKGH                                                       
         NETGO NVPAK,DMCB,SPLPKGN                                               
         OI    SPLPKGNH+6,X'80'                                                 
*                                                                               
         MVI   UPLFLG,0                                                         
         LA    R2,SPLOPTH                                                       
         CLI   5(R2),0                                                          
         BE    VK20                                                             
*                                                                               
* - UPLGLF=0 - SKIP UPLOADED PACKAGES AND UPLOADED UNITS (DEFAULT)              
* - UPLFLG=2 - DO ALL PACKAGES AND UNITS   ('ALL' OPTION)                       
* - UPLFLG=1 - DELETE UPLOADED UNITS, KEEP UPLOADED PKG REC AND                 
*              MARK AS HAVING NO UNITS     ('UPL' OPTION)                       
*                                                                               
         CLC   8(3,R2),=C'UPL'     PKG UPKLOAD OPTION                           
         BNE   *+12                                                             
         MVI   UPLFLG,1            SET UPLOAD FLAG                              
         B     VK20                                                             
         CLC   8(3,R2),=C'ALL'     DO ALL PKG/UNITS                             
         BNE   INVERROR                                                         
         MVI   UPLFLG,2                                                         
*                                                                               
VK20     MVI   FTERMFLG,0          SET REQUIRED FLAG                            
         LA    R2,SPLTSTH                                                       
         NETGO NVGETFLD,DMCB                                                    
         BZ    INVERROR                                                         
         CLI   SPLTST,C'Y'                                                      
         BE    VKEXIT                                                           
         CLI   SPLTST,C'N'                                                      
         BNE   INVERROR                                                         
         MVI   HALF,C'L'           LOCK FOR UPDATIVE SOON                       
         BAS   RE,LOCKEM                                                        
         B     VKEXIT                                                           
*                                                                               
INVERROR MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
VKEXIT   DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
* LOCKER FOR UPDATIVE SOON                                                      
         SPACE                                                                  
LOCKEM   NTR1                                                                   
         CLC   =C'SOON',CONWHEN    IS IT SOON                                   
         BNE   LOCKX               NO/FORGET IT                                 
*        CLC   =C'SJ',NBSELAGY     ONLY FOR SJR                                 
*        BNE   INVERROR                                                         
                                                                                
         CLI   NBSELCLI,X'40'                                                   
         BE    *+14                                                             
         CLC   =C'ALL',NBSELCLI                                                 
         BNE   LOCK0                                                            
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(32),=C'*** UPDATIVE SOON FOR ONE CLIENT'                 
         GOTO1 ERREX2                                                           
* - GET SE NUMBER                                                               
LOCK0    GOTO1 GETFACT,DMCB,(2,0)                                               
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         CLI   FAOVSYS,3           MUST BE NET                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   BYTE,FASYS          GET SE NUMBER                                
*                                                                               
         DROP  R1                                                               
*                                                                               
         CLI   HALF,C'L'           LOCKING?                                     
         BNE   *+8                                                              
         MVI   TWAWHEN,5           SET UPDATIVE SOON                            
* - LOCK / UNLOCK                                                               
         LA    R3,MYKEY                                                         
         USING LKKEYD,R3                                                        
         XC    MYKEY(L'LOCKEY),MYKEY                                            
         MVC   LOCKSE,BYTE         SET SE NUMBER                                
         MVC   LOCKAGY,NBSELAGY    AGENCY                                       
         MVC   LOCKRTY,=C'UN'      UNIT RECORDS                                 
         MVC   LOCKKEY(3),NBSELCLI    3 BYTE CLIENT CODE                        
         MVC   LOCKKEY+3(4),NBSELNET  4 BYTE NETWORK                            
         XC    DMCB(4),DMCB                                                     
         MVC   DMCB+4(3),=X'D9000A'                                             
         MVI   DMCB+7,X'7E'                                                     
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         XC    DMCB(24),DMCB                                                    
         L     R6,ACOMFACS                                                      
         LTR   R6,R6                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTO1 (RF),(R1),(HALF,MYKEY),(R6)                                      
         CLI   DMCB+4,0            ANY ERRORS                                   
         BE    LOCKX                                                            
         XC    CONHEAD,CONHEAD    YES/ERRORS                                    
         MVC   CONHEAD(33),=C'*** CLIENT LOCKED - TRY LATER ***'                
         GOTO1 ERREX2                                                           
*                                                                               
         DROP  R3                                                               
LOCKX    XIT1                                                                   
         EJECT                                                                  
PR       DS    0H                                                               
         XC    DMCB,DMCB                                                        
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD         INIT SORTER                 
         ZAP   TOTDOLS,=P'0'                                                    
         ZAP   TOTDOLS2,=P'0'                                                   
         B     VR3                                                              
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,30,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=110'                                   
         SPACE                                                                  
*                                                                               
* READ LOCKED PKGS AND SET IN TABLE                                             
*                                                                               
VR3      MVI   NBDATA,C'P'         PACKAGES ONLY                                
         MVI   NBSELPST,C'L'       LOCKED PACKAGES ONLY                         
         MVI   NBUSER+13,C'N'      ACCEPT PREEMPTS                              
         MVI   NBRESUME,NBPROCPK                                                
VR3A     NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBMODE,NBREQLST                                                  
         BE    VR3AD                                                            
*                                                                               
         CLI   NBMODE,NBPROCPK                                                  
         BNE   VR3A                                                             
*                                                                               
         L     R2,NBAIO                                                         
         USING NPRECD,R2                                                        
         LA    R1,LOKPKLST         LOCKED PKG LIST                              
         LA    R3,300              ROOM FOR 300 PKGS                            
VR3AA    CLI   0(R1),0             FREE SPACE                                   
         BE    VR3AC                                                            
         LA    R1,1(R1)                                                         
         BCT   R3,VR3AA                                                         
         DC    H'0'                EXPAND TABLE                                 
*                                                                               
VR3AC    MVC   0(1,R1),NPKPACK     ADD PKG TO TABLE                             
         B     VR3A                GET NEXT PKG REC                             
*                                                                               
VR3AD    DS    0H                  END OF READ PKG  REC                         
         DROP  R2                                                               
*                                                                               
                                                                                
*******************************************************                         
*  READ UNITS/TEST IF BILLED/PAYED/DO NOT DELETE THOSE                          
*  NEED TO RAD BOTH LOCKED AND UNLOCKED UNITS SINCE THERE                       
*  MAY BE PKGS THAT ARE LOCKED BUT STILL HAVE SOME UNLOCKED                     
*  UNITS DUE TO SLIPPING THROUGHTHE NET -                                       
*******************************************************                         
         MVI   NBDATA,C'U'         UNITS ONLY                                   
         MVI   NBUSER+13,C'N'      ACCEPT PREEMPTS                              
         MVI   NBSELPST,0          CLEAR LOCKED PKG ONLY OPTION                 
         MVI   NBRESUME,NBPROCPK                                                
VR3AF    NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBMODE,NBREQLST                                                  
         BE    VR5                                                              
*                                                                               
         CLI   NBMODE,NBPROCUN     UNIT ?                                       
         BNE   VR3AF                                                            
*                                                                               
         TM    NBPACKST,X'20'      LOCKED ?                                     
         BO    VR3H                YES-CONTINUE                                 
*                                                                               
* IF UNLOCKED MUST CHECK AGAINST LIST OF LOCKED PKGS                            
* TO ENSURE THIS UNLOCKED UNIT IS NOT ON A LOCKED PKG                           
         LA    R1,LOKPKLST                                                      
         LA    R2,300              MAX NUMBER OF LOCKED PKGS                    
VR3G     CLI   0(R1),0             END OFLIST                                   
         BE    VR3AF               OK -GET NEXT UNIT                            
         CLC   NBPACK,0(R1)                                                     
         BE    VRPKERR             ERROR - UNIT NOT LOCKED                      
         LA    R1,1(R1)                                                         
         BCT   R2,VR3G             IF END OF LIST - OK                          
VR3GG    B     VR3AF               UNLOCKED UNIT NOT ON PKGLOK LIST             
*                                                                               
                                                                                
*                                                                               
VR3H     TM    NBPACKST,X'02'      IF AUDIT TRAIL ON                            
         BO    VRPKERR             SKIP                                         
*                                                                               
         MVI   ELCODE,X'10'        BILLING OLD                                  
         L     R6,NBAIO                                                         
         USING NUPAYD,R6                                                        
         BAS   RE,GETEL                                                         
         BE    VRPKERR                                                          
*                                                                               
         XC    KEY2,KEY2                                                        
         LA    R1,KEY2                                                          
         USING NUBKEY,R1                                                        
         MVC   KEY2(2),=X'0E06'                                                 
         MVC   NUBKAM,NBACTAM                                                   
         MVC   NUBKCLI,NBACTCLI                                                 
         MVC   NUBKNET,NBACTNET                                                 
         MVC   NUBKPROG,NBACTPRG                                                
         MVC   NUBKDATE,NBACTDAT                                                
         MVC   NUBKEST,NBACTEST                                                 
         MVC   NUBKSUB,NBACTSUB                                                 
         MVC   NUBKDPT,NBACTDP                                                  
         MVC   KEY2SV,KEY2                                                      
         GOTO1 NBDM,DMCB,(0,=C'DMRDHI  '),=C'XSPDIR  ',KEY2,KEY2,0              
         CLC   KEY2SV(20),KEY2                                                  
         BE    VRPKERR             YES-BILLING                                  
         DROP  R1                                                               
         DROP  R6                                                               
*                                                                               
VR3D     MVI   ELCODE,X'12'        PAYING ELEMENTS                              
         L     R6,NBAIO                                                         
         USING NUPAYD,R6                                                        
         BAS   RE,GETEL                                                         
         BE    VRPKERR                                                          
         CLI   UPLFLG,0            DEFAULT SKIPS UPLOADED PKG/UNITS             
         BNE   VR3X                                                             
*                                                                               
         L     R6,NBAIO            YES/FIND UPLOADED AND SKIP                   
         MVI   ELCODE,X'02'        (IN REPORT UPLOADED WILL APPEAR              
         BAS   RE,GETEL             WITH BILL/PAID ERROR SINCE THIS             
         BNE   VR3X                 IS ONLY ERROR PRINTING SO FAR)              
         USING NUSDRD,R6                                                        
         TM    NUSDST3,X'04'       CABLE UPLOADED UNIT                          
         BO    VRPKERR             YES/SKIP THIS PACKAGE                        
         B     VR3X                                                             
VRPKERR  LA    R1,PACKSV           NO/SAVE PKG NUMBER                           
         LA    R2,50                  FOR A MAX OF 50 PKG NUMBERS               
VR3J     CLI   0(R1),0                                                          
         BE    VR3L                                                             
         CLC   0(1,R1),NBPACK                                                   
         BE    VR3X                                                             
         LA    R1,1(R1)                                                         
         BCT   R2,VR3J                                                          
         DC    H'0'                                                             
VR3L     DS    0H                                                               
         MVC   0(1,R1),NBPACK                                                   
VR3X     DS    0H                                                               
         B     VR3AF                   GET NEXT UNIT                            
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
*     RE-READ PACKAGES AND UNITS/ TURN ON NEVER-SHOW BIT                        
VR5      MVI   NBDATA,C'B'                                                      
         MVI   NBSELPST,C'L'              LOCKED PACKAGES ONLY                  
         MVI   NBRESUME,NBPROCPK                                                
         LA    R1,NETIOHK                                                       
         ST    R1,NBHOOK                                                        
VR10     NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBMODE,NBREQLST     LAST REQ                                     
         BE    WRTREP              YES/GET SORTRECS, WRITE REPORT               
         B     VR10                NO/GET NEXT UNIT                             
*                                                                               
NETIOHK  NTR1                                                                   
         MVI   NBNOWRIT,C'N'                                                    
         MVI   NBUPUNIT,C'N'                                                    
*                                                                               
         CLI   NBMODE,NBPROCPK                                                  
         BE    NK20                                                             
         CLI   NBMODE,NBPROCUN                                                  
         BE    NK30                                                             
         B     NETHKX                                                           
*                                                                               
NK20     DS    0H                       ** PACKAGES **                          
         L     R6,NBAIO                                                         
         USING NPRECD,R6                                                        
         LA    R1,20                                                            
         LA    R2,PACKSV                                                        
         LA    R3,50                                                            
NK22     CLC   NPKPACK,0(R2)                                                    
         BE    PKGERR                                                           
         LA    R2,1(R2)                                                         
         BCT   R3,NK22                                                          
         B     NK24                                                             
*                                                                               
PKGERR   DS    0H            LOCKED PACKAGE HAS BILLED/PAID                     
         XC    P(100),P                                                         
         MVC   P(2),=X'0001'                                                    
         MVC   P+2(3),NBCLICOD            CLIENT                                
         EDIT  (B1,NBACTPAK),(3,P+7)    PACKAGE NUMBER                          
         MVC   P+12(4),NBACTNET         NETWORK                                 
         EDIT  (B1,NBACTEST),(3,P+18)   ESTIMATE                                
         MVC   P+34(16),NBPAKNAM        PACKAGE NAME                            
         MVC   P+55(35),=C' *** PACKAGE HAS BILL/PAY/UPLD  ***'                 
         B     PUTSRT                                                           
*                                                                               
NK24     MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NPAKEL,R6                                                        
         CLI   UPLFLG,1            IS IT PKG UPLOAD OPTION?                     
         BNE   NK26                NO                                           
         OI    NPAKCNTL,X'20'      YES  .SET NO UNITS UNDER PACKAGE             
         NI    NPAKCNTL,X'FF'-X'10'     .TURN OFF CABLE UPLOAD PKG              
         OI    NPAKCNTL,X'08'           .TURN ON PKG CABLE LOCKED               
         NI    NPAKSTAT,X'FF'-X'20'     .TURN OFF LOCKED BIT                    
         CLI   SPLTST,C'N'         IS IT A TEST?                                
         BNE   NK40                YES                                          
         MVI   NBUPUNIT,C'Y'       NO/   TURN ON WRITE FLAGS                    
         MVI   NBNOWRIT,C'Y'                                                    
         B     NK40                                                             
*                                                                               
*NK26     OI    NPAKSTAT,X'01'      TURN ON NEVER SHOW BIT                      
NK26     L     R6,NBAIO            DELETE AS OF 10/28/96                        
         USING NPRECD,R6                                                        
         OI    NPKRSTAT,X'80'                                                   
*                                                                               
NK27     CLI   SPLTST,C'N'         IS IT A TEST                                 
         BNE   NK40                                                             
         MVI   NBUPUNIT,C'Y'       NO/   TURN ON WRITE FLAGS                    
         MVI   NBNOWRIT,C'Y'                                                    
         MVC   MYKEY,0(R6)          AND DELETE PACKAGE RECORD                   
         MVC   KEY,MYKEY                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(20),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   MYKEY,KEY            * SET KEY WITH DISK ADDR                    
         OI    MYKEY+20,X'80'                                                   
         BAS   RE,WRTDIR                                                        
*                                                                               
         B     NK40                                                             
         DROP  R6                                                               
*                                                                               
NK30     DS    0H                    ** UNIT RECORDS **                         
         L     R6,NBAIO                                                         
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NUMAINEL,R6                                                      
*                                                                               
         LA    R2,PACKSV           CHK IF UNIT BELONGS TO PAID/BILLED           
         LA    R1,50               LOCKED PACKAGE                               
NK32     CLC   NBPACK,0(R2)                                                     
         BE    NETHKX              YES/SKIP IT                                  
         LA    R2,1(R2)                                                         
         BCT   R1,NK32                                                          
*                                                                               
**       OI    NUPACKST,X'01'      TURN ON NEVER SHOW BIT                       
**       CLI   UPLFLG,1            ...CHK UPLOAD                                
**       BNE   NK34                AS OF 10/28/96 DELETE ALL                    
         L     R6,NBAIO                                                         
         USING NURECD,R6                                                        
         OI    NURSTAT,X'80'       ...DELETE IT                                 
NK34     CLI   SPLTST,C'N'         IS IT A TEST                                 
         BNE   NK40                                                             
         MVI   NBUPUNIT,C'Y'       NO/ TURN ON WRITE FLAGS                      
         MVI   NBNOWRIT,C'Y'                                                    
*                                                                               
         EJECT                                                                  
*                                                                               
*        WRITE BACK KEYS                                                        
*                                                                               
*                                                                               
         DROP  R6                                                               
         LA    R1,MYKEY            PASSIVE KEY                                  
         USING NUKPKEY,R1                                                       
         MVC   MYKEY,NBKEY                                                      
         XC    MYKEY+4(16),MYKEY+4                                              
         MVI   MYKEY,X'84'                                                      
         MVC   NUKPNET,NBACTNET                                                 
         MVC   NUKPPROG,NBACTPRG                                                
         MVC   NUKPDATE,NBACTDAT                                                
         MVC   NUKPEST,NBACTEST                                                 
         MVC   NUKPSUB,NBACTSUB                                                 
         MVC   NUKPDP,NBACTDP                                                   
         MVC   KEY(20),MYKEY                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(20),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FULL,KEY+21         SAVE DISKADDRESS                             
         OI    MYKEY+20,X'80'                                                   
         BAS   RE,WRTDIR                                                        
*                                                                               
         USING NUKDKEY,R1            SECOND PASSIVE KEY                         
         LA    R1,MYKEY                                                         
         XC    MYKEY+4(16),MYKEY+4                                              
         MVI   MYKEY,X'94'                                                      
         MVC   NUKDEST,NBACTEST                                                 
         MVC   NUKDNET,NBACTNET                                                 
         BAS   RE,DAYLOOK          RETURNS ONE BYTE DAY CODE IN BYTE            
         MVC   NUKDDAY,BYTE                                                     
         MVC   NUKDTIME,NBACTSQH                                                
         MVC   NUKDPROG,NBACTPRG                                                
         MVC   NUKDDATE,NBACTDAT                                                
         MVC   NUKDSUB,NBACTSUB                                                 
         MVC   KEY(20),MYKEY                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(20),KEYSAVE                                                  
         BE    WRTD10                                                           
         MVI   NUKDDAY,0           TRY 0 DAY                                    
         MVC   KEY(20),MYKEY                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(20),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
WRTD10   CLC   FULL,KEY+21         DISK ADDRESSES THE SAME                      
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    MYKEY+20,X'80'                                                   
         BAS   RE,WRTDIR                                                        
*                                                                               
         LA    R1,MYKEY            DELETE MAIN KEY LAST                         
         MVC   MYKEY,NBKEY         TO PRESERVE SEQ NETIO READ                   
         MVC   KEY,MYKEY                                                        
         GOTO1 HIGH                                                             
         OI    MYKEY+20,X'80'                                                   
         BAS   RE,WRTDIR                                                        
         B     NK40                                                             
*                                                                               
WRTDIR   NTR1                                                                   
         DS    0H                                                               
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'UNTDIR  ',MYKEY,MYKEY                  
         B     WRTDX                                                            
         GOTO1 HEXOUT,DMCB,MYKEY,P,40,       ** TESTING **                      
         BAS   RE,SPOOLIT                                                       
WRTDX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
DAYLOOK  NTR1                                                                   
         LA    R4,DAYLKUP                                                       
DLK2     CLC   0(3,R4),NBDAYNAM                                                 
         BE    DLK5                                                             
         LA    R4,4(R4)                                                         
         CLI   0(R4),X'FF'                                                      
         BNE   DLK2                                                             
         DC    H'0'                                                             
DLK5     MVC   BYTE,3(R4)                                                       
         XIT1                                                                   
         DROP  R1                                                               
*                                                                               
DAYLKUP  EQU   *                                                                
         DC    CL3'ALL',XL1'FF'                                                 
         DC    CL3'M-F',XL1'08'                                                 
         DC    CL3'MON',XL1'01'                                                 
         DC    CL3'TUE',XL1'02'                                                 
         DC    CL3'WED',XL1'03'                                                 
         DC    CL3'THU',XL1'04'                                                 
         DC    CL3'FRI',XL1'05'                                                 
         DC    CL3'SAT',XL1'06'                                                 
         DC    CL3'SUN',XL1'07'                                                 
         DC    CL3'M-S',XL1'09'                                                 
         DC    CL3'VAR',XL1'0A'                                                 
         DC    XL3'FFFFFF',CL1' '      END OF TABLE                             
         EJECT                                                                  
*                                                                               
NK40     DS    0H                                                               
         B     NK35                                                             
         GOTO1 HEXOUT,DMCB,NBAIO,P,140 ******  FOR TESTING ONLY                 
         BAS   RE,SPOOLIT                                                       
*                                                                               
NK35     DS    0H                                                               
         XC    P(100),P                                                         
         MVC   P(3),NBCLICOD            CLIENT                                  
         EDIT  (B1,NBACTPAK),(3,P+5)    PACKAGE NUMBER                          
         MVC   P+10(4),NBACTNET         NETWORK                                 
         EDIT  (B1,NBACTEST),(3,P+16)   ESTIMATE                                
         CLI   NBMODE,NBPROCPK                                                  
         BE    NK37                                                             
         EDIT  (B1,NBPACK),(3,P+5)      PACKAGE NUMBER                          
         MVC   P+22(6),NBACTPRG                                                 
         MVC   P+30(16),NBPROGNM                                                
         GOTO1 DATCON,DMCB,(2,NBACTDAT),(5,P+48)                                
         EDIT  (B1,NBACTSUB),(2,P+57)                                           
         MVI   P+56,C'-'                                                        
         EDIT  (B4,NBACTUAL),(10,P+68),2  ACTUAL COST                           
         L     R1,NBACTUAL                                                      
         CVD   R1,DUB                                                           
         AP    TOTDOLS,DUB                                                      
         AP    TOTDOLS2,DUB                                                     
         L     R1,NBINTEG                                                       
         CVD   R1,DUB                                                           
         AP    TOTDOLS2,DUB                                                     
                                                                                
* ADD SPECIAL CHARGES TO TOTDOLS                                                
         L     R6,NBAIO                                                         
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
NK35C    BAS   RE,NEXTEL                                                        
         BNE   NK36                                                             
         USING NUSPRD,R6                                                        
         ICM   R1,15,NUSPRAMT                                                   
         CVD   R1,DUB                                                           
         AP    TOTDOLS,DUB                                                      
         B     NK35C                                                            
         DROP  R6                                                               
                                                                                
NK36     B     PUTSRT                                                           
*                                                                               
NK37     MVC   P+32(16),NBPAKNAM        PACKAGE NAME                            
         EDIT  NBPAKCST,(10,P+50)  PACKAGE COST                                 
         MVI   P+75,C'*'           SET PKG INDICATOR                            
PUTSRT   GOTO1 SORTER,DMCB,=C'PUT',P                                            
NETHKX   XIT1                                                                   
         SPACE                                                                  
*                                                                               
WRTREP   DS    0H                   GET RECS FROM SORT/WRITE REPORT             
         XC    P,P                                                              
GTSRT    GOTO1 SORTER,DMCB,=C'GET'                                              
         L     R3,4(R1)                                                         
         LTR   R3,R3                                                            
         BZ    WRTX                                                             
         CLC   PREVREC,0(R3)     ON CHANGE OF KEY/SKIP LINE                     
         BE    *+8                                                              
         BAS   RE,SPOOLIT                                                       
         MVC   P+20(110),0(R3)                                                  
         CLI   75(R3),C'*'         IS IT A PKG REC                              
         BNE   WRTIT                                                            
*                         PKG RECS GET PRINTED DIFFERENTLY                      
         XC    P,P                                                              
         MVC   P(7),=C'PACKAGE'                                                 
         MVC   P+8(3),5(R3)        PKG NUM                                      
         MVC   P2(11),=11C'-'                                                   
         MVC   P+13(4),10(R3)      NETWORK                                      
         MVC   P+19(16),32(R3)     PKG NAME                                     
         MVC   P+37(10),50(R3)     PKG COST                                     
*                                                                               
WRTIT    DS    0H                                                               
         CLI   0(R3),0             IS IT PKG ERR REC                            
         BNE   WRT5                                                             
         MVC   P+20(110),P+22                                                   
WRT5     BAS   RE,SPOOLIT                                                       
         CLC   PREVREC,0(R3)                                                    
         BE    GTSRT                                                            
         MVC   PREVREC,0(R3)                                                    
         B     GTSRT                                                            
WRTX     DS    0H                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   P+5(14),=C'*** TOTALS ***'                                       
         EDIT  (P8,TOTDOLS),(15,P+83),2                                         
         EDIT  (P8,TOTDOLS2),(15,P+215),2                                       
         MVC   P+232(23),=C'(ORDERED + INTEGRATION)'                            
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         L     RE,BOOKVAL       ADD FOR ALL AGENCY RUN TOTALS                   
         AP    0(8,RE),TOTDOLS                                                  
         AP    8(8,RE),TOTDOLS2                                                 
*                                                                               
         CLI   SPLTST,C'Y'         IF TEST                                      
         BE    EXIT                THAT'S ALL                                   
         MVI   HALF,C'U'           ELSE/UNLOCK IF SOON                          
         BAS   RE,LOCKEM                                                        
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
         GETEL (R6),NBDTADSP,ELCODE                                             
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
*                                                                               
SPOOLIT  NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
HDHOOK   NTR1                                                                   
         MVC   H1(12),=C'NETWORK T.V.'                                          
         MVC   H3(6),=C'CLIENT'                                                 
         MVC   H3+10(3),SPLCLI                                                  
         MVC   H3+15(20),SPLCLIN                                                
         MVC   H4(8),=C'ESTIMATE'                                               
         MVC   H4+10(3),SPLEST                                                  
         MVC   H4+15(20),SPLESTN                                                
         MVC   H5(7),=C'PACKAGE'                                                
         MVC   H5+9(2),SPLPKG                                                   
         MVC   H5+15(20),SPLPKGN                                                
         MVI   H8,C' '                                                          
         MVI   H9,C' '                                                          
*                                                                               
         MVC   H10+20(3),=C'CLT'                                                
         MVC   H11+20(3),=10C'-'                                                
         MVC   H10+25(3),=C'PKG'                                                
         MVC   H11+25(3),=10C'-'                                                
         MVC   H10+30(4),=C'NTWK'                                               
         MVC   H11+30(4),=10C'-'                                                
         MVC   H10+36(3),=C'EST'                                                
         MVC   H11+36(3),=10C'-'                                                
         MVC   H10+41(4),=C'PROG'                                               
         MVC   H11+41(4),=10C'-'                                                
         MVC   H10+68(4),=C'DATE'                                               
         MVC   H11+68(4),=10C'-'                                                
         MVC   H10+88(8),=C'ACT COST'                                           
         MVC   H11+88(10),=10C'-'                                               
HDX      B     EXIT                 (XIT1)                                      
*                                                                               
HEADING  SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,52,C'LOCKED PACKAGE DELETION REPORT'                          
         SSPEC H2,52,C'------------------------------'                          
         SSPEC H1,93,AGYNAME                                                    
         SSPEC H2,93,AGYADD                                                     
         SSPEC H3,93,REPORT                                                     
         SSPEC H4,93,RUN                                                        
         SSPEC H5,103,RUN                                                       
         DC    X'00'                                                            
         EJECT                                                                  
         LTORG                                                                  
         SPACE                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
WORKD    DSECT                                                                  
RELO     DS    F                                                                
ACLISTSV DS    F                                                                
BILLTOT  DS    F                                                                
PAYTOT   DS    F                                                                
FRST     DS    CL1                                                              
UPLFLG   DS    CL1                                                              
BOXSET   DS    CL1                                                              
PREVREC  DS    CL13                                                             
TOTDOLS  DS    CL8                 ORDERED DOLLARS                              
TOTDOLS2 DS    CL8                 ORDERED + INTEGRATION                        
PACKSV   DS    CL50                                                             
MYKEY    DS    CL30                                                             
KEY2     DS    CL40                                                             
KEY2SV   DS    CL40                                                             
LOKPKLST DS    CL300                                                            
         EJECT                                                                  
       ++INCLUDE NETINCLS                                                       
       ++INCLUDE NEGENUBILL                                                     
         EJECT                                                                  
       ++INCLUDE NEMEDFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDDBD                                                       
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
       ++INCLUDE DDBIGBOX                                                       
         EJECT                                                                  
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE NEGENPACK                                                      
       ++INCLUDE FALOCKETD                                                      
       ++INCLUDE FASYSFAC                                                       
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FASSB                                                          
       ++INCLUDE DDMASTC                                                        
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'113NEMEDCC   03/03/09'                                      
         END                                                                    
