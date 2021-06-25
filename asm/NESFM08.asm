*          DATA SET NESFM08    AT LEVEL 203 AS OF 11/13/06                      
*PHASE T31C08A,*                                                                
***********************************************************************         
*                                                                               
*  TITLE: T31C08 - MAINTENANCE/LIST OF DAYPART RECORDS                          
*                                                                               
*  CALLS TO:    DATAMGR                                                         
*                                                                               
*  INPUTS: SCREENS NESFMA8 (T31CA8) -- MAINTENANCE                              
*                  NESFMA9 (T31CA9) -- LIST                                     
*                                                                               
*  OUTPUTS: UPDATED OR NEW DAYPARTS                                             
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
*   HIST:  (PLEASE INSERT BRIEF DESCRIPTION OF UPDATES)                         
*                                                                               
***********************************************************************         
         TITLE 'T31C08 NETWORK DAPYART RECORD'                                  
T31C08   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,NETDPT,R7                                                      
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
         MVI   USEIO,C'Y'          CONTROL MY OWN ADDREC/PUTREC                 
*                                                                               
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
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE KEY FOR LIST                                                         
***********************************************************************         
VKLIST   DS    0H                                                               
         CLI   ACTNUM,ACTDEL                                                    
         BE    INVLACT                                                          
         CLI   ACTNUM,ACTREST                                                   
         BE    INVLACT                                                          
*                                                                               
         XC    TEMPFLD,TEMPFLD                                                  
         MVC   TEMPFLD,=XL9'0900000000010000D5'                                 
         LA    R2,TEMPFLD                                                       
         GOTO1 VALIMED                                                          
*                                                                               
         XC    BCLT,BCLT                                                        
         XC    TMPDPTA,TMPDPTA                                                  
*****************************************************************               
* REMOVE THIS AFTER CLT LEVEL GOES LIVE                                         
*****************************************************************               
*&&DO                                                                           
         LA    R2,DPLCLTH                                                       
         CLI   5(R2),0                                                          
         BE    *+14                                                             
         CLC   =C'***',8(R2)                                                    
         BNE   INVLFLD                                                          
*                                                                               
         LA    R2,DPLDPTH                                                       
         CLI   5(R2),0                                                          
         BE    VKL30                                                            
         B     INVLFLD                                                          
*&&                                                                             
*****************************************************************               
         LA    R2,DPLCLTH                                                       
         CLI   5(R2),0                                                          
         BE    VKL20                                                            
*                                                                               
         CLC   =C'###',8(R2)                                                    
         BE    VKL20                                                            
         CLC   =C'***',8(R2)                                                    
         BE    VKL20                                                            
*                                                                               
         GOTO1 VALICLT                                                          
*                                                                               
VKL20    DS    0H                                                               
         LA    R2,DPLDPTH                                                       
         CLI   5(R2),0                                                          
         BE    VKL30                                                            
*                                                                               
         MVC   TMPDPTA,8(R2)       SAVE AWAY 2 CHAR ALPHA DPT                   
         OC    TMPDPTA,SPACES                                                   
*                                                                               
VKL30    DS    0H                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING NDPTHDR,R4                                                       
*                                                                               
         MVC   NDPTKEY(2),=XL2'0D07'                                            
         MVC   NDPTAGM,BAGYMD      AGENCY/MEDIA                                 
*                                                                               
         LA    R2,DPLCLTH                                                       
         XC    SAVEKEY,SAVEKEY                                                  
         MVC   SAVEKEY,KEY                                                      
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(3),SAVEKEY                                                   
         BNE   NODPTLST                                                         
*                                                                               
VKLX     DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE KEY                                                                  
***********************************************************************         
VK       DS    0H                                                               
         CLI   ACTNUM,ACTDEL                                                    
         BE    INVLACT                                                          
         CLI   ACTNUM,ACTREST                                                   
         BE    INVLACT                                                          
*****************************************************************               
* REMOVE THIS AFTER CLT LEVEL GOES LIVE                                         
*****************************************************************               
*&&DO                                                                           
         CLI   ACTNUM,ACTCHA                                                    
         BE    INVLACT                                                          
*&&                                                                             
*****************************************************************               
*                                                                               
         XC    TEMPFLD,TEMPFLD                                                  
         MVC   TEMPFLD,=XL9'0900000000010000D5'                                 
         LA    R2,TEMPFLD                                                       
         GOTO1 VALIMED                                                          
*                                                                               
         CLI   ACTNUM,ACTLIST                                                   
         BNE   *+12                                                             
         BAS   RE,VKLIST                                                        
         B     VKX                                                              
*****************************************************************               
* REMOVE THIS AFTER CLT LEVEL GOES LIVE                                         
*****************************************************************               
*&&DO                                                                           
         LA    R2,DPTCLTH                                                       
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         CLC   =C'***',8(R2)                                                    
         BNE   INVLFLD                                                          
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BNE   VK30                                                             
*                                                                               
         LA    R2,DPTDPTH                                                       
         CLI   5(R2),0                                                          
         BE    *+8                                                              
         B     INVLFLD                                                          
*                                                                               
         BAS   RE,ADDAGYL          YES - ADD ALL AGY LEVEL DAYPARTS             
         B     VKX                                                              
*&&                                                                             
*****************************************************************               
         LA    R2,DPTCLTH                                                       
         CLC   =C'***',8(R2)       ADD AGY LEVEL DAYPARTS?                      
         BNE   *+8                 NO                                           
         BAS   RE,AGYLEV           CHECK IF AGY LEVEL DPTS EXIST                
*                                                                               
         LA    R2,DPTCLTH                                                       
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
*                                                                               
         CLC   =C'###',8(R2)       ADD ALL AGY LEVEL DAYPARTS?                  
         BNE   *+12                                                             
         BAS   RE,ADDAGYL                                                       
         B     VKX                                                              
*                                                                               
         CLC   =C'***',8(R2)       ADD SINGLE AGY LEVEL DAYPART?                
         BNE   VK20                NO                                           
         XC    BCLT,BCLT                                                        
*                                                                               
         LA    R2,DPTDPTH                                                       
         CLI   5(R2),0             MUST HAVE A DAYPART ENTERED                  
         BE    MISSFLD                                                          
         B     VK30                                                             
*                                                                               
VK20     DS    0H                                                               
         GOTO1 VALICLT                                                          
*                                                                               
VK30     DS    0H                                                               
         LA    R2,DPTDPTH                                                       
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
*                                                                               
         MVC   TMPDPTA,8(R2)       SAVE AWAY 2 CHAR ALPHA DPT                   
         OC    TMPDPTA,SPACES                                                   
*                                                                               
         CLI   TMPDPTA,C'A'                                                     
         BL    INVLFLD                                                          
         CLI   TMPDPTA,C'9'                                                     
         BH    INVLFLD                                                          
*                                                                               
         CLI   TMPDPTA+1,C' '                                                   
         BE    VK35                                                             
         CLI   TMPDPTA+1,C'A'                                                   
         BL    INVLFLD                                                          
         CLI   TMPDPTA+1,C'9'                                                   
         BH    INVLFLD                                                          
*                                                                               
VK35     XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING NDPTHDR,R4                                                       
*                                                                               
         MVC   NDPTKEY(2),=XL2'0D07'                                            
         MVC   NDPTAGM,BAGYMD      AGENCY/MEDIA                                 
         MVC   NDPTCLT,BCLT        CLIENT                                       
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BNE   *+12                                                             
         BAS   RE,CHKDPT           CHECK IF THIS DPT ALREADY EXISTS             
         B     VKX                                                              
*                                                                               
         BAS   RE,GETDPTE          GET DAYPART EQUATE                           
         MVC   NDPTDPTE,TMPDPTE    GOT DAYPART EQUATE                           
*                                                                               
         LA    R2,DPTCLTH                                                       
         XC    SAVEKEY,SAVEKEY                                                  
         MVC   SAVEKEY,KEY                                                      
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(6),SAVEKEY                                                   
         BNE   INVLNFND                                                         
*                                                                               
VKX      DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* CHECK IF DAYPART ALREADY EXISTS AT AGY/CLT LEVEL                              
***********************************************************************         
CHKDPT   NTR1                                                                   
         MVC   SAVEKEY,KEY                                                      
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING NDPTHDR,R4                                                       
*                                                                               
         MVC   NDPTKEY(2),=XL2'0D07'                                            
         MVC   NDPTAGM,BAGYMD      AGENCY/MEDIA                                 
*                                                                               
         GOTO1 HIGH                                                             
         B     CHKDPT20                                                         
*                                                                               
CHKDPT10 DS    0H                                                               
         GOTO1 SEQ                                                              
*                                                                               
CHKDPT20 CLC   KEY(3),SAVEKEY                                                   
         BNE   CHKDPT40            UNIQUE TO AGY, NOW CHECK CLT LEVEL           
*                                                                               
         OC    NDPTCLT,NDPTCLT                                                  
         BNZ   CHKDPT40                                                         
*                                                                               
         CLC   NDPTDPTA,TMPDPTA    FOUND MATCH?                                 
         BE    DUPADPT             YES - DPT ALREADY EXISTS                     
         B     CHKDPT10                                                         
*                                                                               
CHKDPT40 DS    0H                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING NDPTHDR,R4                                                       
*                                                                               
         MVC   NDPTKEY(2),=XL2'0D07'                                            
         MVC   NDPTAGM,BAGYMD      AGENCY/MEDIA                                 
         MVC   NDPTCLT,BCLT        CLIENT                                       
         MVI   NDPTCLT+2,X'01'     GET FIRST CLIENT                             
*                                                                               
         GOTO1 HIGH                                                             
         B     CHKDPT60                                                         
*                                                                               
CHKDPT50 DS    0H                                                               
         GOTO1 SEQ                                                              
*                                                                               
CHKDPT60 CLC   KEY(3),SAVEKEY                                                   
         BNE   CHKDPT80            UNIQUE TO CLIENT, DPT OK                     
*                                                                               
         CLC   DPTCLT,=C'***'                                                   
         BNE   CHKDPT65                                                         
*                                                                               
         OC    NDPTCLT,NDPTCLT                                                  
         BZ    CHKDPT50                                                         
         B     CHKDPT70                                                         
*                                                                               
CHKDPT65 CLC   NDPTCLT,BCLT                                                     
         BNE   CHKDPT50                                                         
*                                                                               
CHKDPT70 CLC   NDPTDPTA,TMPDPTA    FOUND MATCH?                                 
         BE    DUPCDPT             YES - DPT ALREADY EXISTS                     
         B     CHKDPT50                                                         
*                                                                               
CHKDPT80 DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(20),SAVEKEY                                                  
*                                                                               
CHKDPTX  DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET DAYPART EQUATE                                                            
***********************************************************************         
GETDPTE  NTR1                                                                   
         LA    R4,KEY                                                           
         USING NDPTHDR,R4                                                       
*                                                                               
         MVC   NDPTKEY(2),=XL2'0D07'                                            
         MVC   NDPTAGM,BAGYMD      AGENCY/MEDIA                                 
         MVC   NDPTCLT,BCLT        CLIENT                                       
*                                                                               
         LA    R2,DPTCLTH                                                       
         XC    SAVEKEY,SAVEKEY                                                  
         MVC   SAVEKEY,KEY                                                      
*                                                                               
         GOTO1 HIGH                                                             
GDPTE10  CLC   KEY(5),SAVEKEY                                                   
         BNE   INVLNFND                                                         
*                                                                               
         CLC   NDPTDPTA,TMPDPTA    SAME DAYPART                                 
         BE    GDPTE20                                                          
*                                                                               
         GOTO1 SEQ                                                              
         B     GDPTE10                                                          
*                                                                               
GDPTE20  DS    0H                                                               
         MVC   TMPDPTE,NDPTDPTE    SAVE DAYPART EQUATE                          
         XC    KEY,KEY                                                          
         MVC   KEY(20),SAVEKEY                                                  
*                                                                               
GETDPTEX DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ADD AGENCY LEVEL DAYPARTS                                                     
***********************************************************************         
ADDAGYL  NTR1                                                                   
         LA    R3,DAYPARTS                                                      
*                                                                               
AAGYL10  DS    0H                                                               
         CLI   0(R3),X'FF'         ADDED ALL DAYPARTS?                          
         BE    ADDAGYM                                                          
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING NDPTHDR,R4                                                       
*                                                                               
         MVC   NDPTKEY(2),=XL2'0D07'                                            
         MVC   NDPTAGM,BAGYMD      AGENCY/MEDIA                                 
         MVC   NDPTCLT,=XL2'0000'  CLIENT                                       
*                                                                               
         MVC   NDPTDPTE,0(R3)      DAYPART MAP EQUATE                           
*                                                                               
         GOTO1 HIGH                DOES IT ALREADY EXIST?                       
         CLC   KEY(6),SAVEKEY                                                   
         BE    AAGYL50                                                          
*                                                                               
         XC    KEY,KEY                                                          
*                                                                               
         MVC   NDPTKEY(2),=XL2'0D07'                                            
         MVC   NDPTAGM,BAGYMD      AGENCY/MEDIA                                 
         MVC   NDPTCLT,=XL2'0000'  CLIENT                                       
*                                                                               
         MVC   NDPTDPTE,0(R3)      DAYPART MAP EQUATE                           
*                                                                               
         MVC   NDPTDES(8),2(R3)    DESCRIPTION                                  
         OC    NDPTDES,SPACES                                                   
         MVC   NDPTDPTA,0(R3)      ALPHA DAYPART                                
*                                                                               
         MVC   NDPTDPTA+2(2),=XL2'FFFF'                                         
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'UNTDIR  ',KEY,KEY                      
*                                                                               
AAGYL50  DS    0H                                                               
         LA    R3,10(R3)           BUMP TO NEXT DAYPART IN TABLE                
         B     AAGYL10                                                          
*                                                                               
ADDAGYLX DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* CHECK IF AGENCY LEVEL DAYPARTS ADDED YET                                      
***********************************************************************         
AGYLEV   NTR1                                                                   
         LA    R2,DPTCLTH                                                       
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING NDPTHDR,R4                                                       
*                                                                               
         MVC   NDPTKEY(2),=XL2'0D07'                                            
         MVC   NDPTAGM,BAGYMD      AGENCY/MEDIA                                 
*                                                                               
         XC    SAVEKEY,SAVEKEY                                                  
         MVC   SAVEKEY,KEY                                                      
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(3),SAVEKEY                                                   
         BNE   AGYERR                                                           
         CLC   KEY+3(2),=XL2'0000'                                              
         BNE   AGYERR                                                           
*                                                                               
AGYLEVX  DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* RESTORE RECORD                                                                
***********************************************************************         
RESTREC  NTR1                                                                   
         LA    R4,KEY                                                           
         USING NDPTHDR,R4                                                       
*                                                                               
         NI    NDPTCNTL,X'FF'-X'80'  MARK UNDELETED                             
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'UNTDIR  ',KEY,KEY                      
*                                                                               
RESTRECX DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DELETE RECORD                                                                 
***********************************************************************         
DELREC   NTR1                                                                   
         LA    R4,KEY                                                           
         USING NDPTHDR,R4                                                       
*                                                                               
         OI    NDPTCNTL,X'80'      MARK FOR DELETION                            
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'UNTDIR  ',KEY,KEY                      
*                                                                               
DELRECX  DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY KEY                                                                   
***********************************************************************         
DK       DS    0H                                                               
         LA    R4,KEY                                                           
         USING NDPTHDR,R4                                                       
*                                                                               
         MVC   SAVEKEY,KEY                                                      
*                                                                               
         OC    NDPTCLT,NDPTCLT     AGENCY LEVEL                                 
         BNZ   DK20                                                             
         MVC   DPTCLT,=C'***'                                                   
         OI    DPTCLTH+6,X'80'                                                  
         B     DK30                                                             
*                                                                               
DK20     DS    0H                                                               
         XC    BYTE,BYTE                                                        
         GOTO1 CLUNPK,DMCB,(BCLIAAN,NDPTCLT),DPTCLT                             
         OI    DPTCLTH+6,X'80'                                                  
         MVC   BCLT,NDPTCLT                                                     
*                                                                               
DK30     DS    0H                                                               
         MVC   DPTDPT,NDPTDPTA     DISPLAY DAYPART                              
         OI    DPTDPTH+6,X'80'                                                  
*                                                                               
         MVC   DPTDESC,NDPTDES     DISPLAY DESCRIPTION                          
         OI    DPTDESCH+6,X'80'                                                 
*                                                                               
DKX      DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE RECORD                                                               
***********************************************************************         
VR       DS    0H                                                               
         CLI   ACTNUM,ACTDEL                                                    
         BE    INVLACT                                                          
         CLI   ACTNUM,ACTREST                                                   
         BE    INVLACT                                                          
*****************************************************************               
* REMOVE THIS AFTER CLT LEVEL GOES LIVE                                         
*****************************************************************               
*&&DO                                                                           
         CLI   ACTNUM,ACTCHA                                                    
         BE    INVLACT                                                          
*&&                                                                             
*****************************************************************               
         LA    RF,2000                                                          
         L     RE,AIO1                                                          
         XCEF                                                                   
*                                                                               
         LA    R4,KEY                                                           
         USING NDPTHDR,R4                                                       
*                                                                               
         MVC   NDPTDES,DPTDESC     MOVE IN DESCRIPTION TO KEY                   
         OC    NDPTDES,SPACES                                                   
*                                                                               
         MVC   NDPTKEY(2),=XL2'0D07'                                            
         MVC   NDPTAGM,BAGYMD      AGENCY/MEDIA                                 
         MVC   NDPTCLT,BCLT        CLIENT                                       
         MVC   NDPTDPTA,DPTDPT     ALPHA DAYPART                                
         OC    NDPTDPTA,SPACES                                                  
         MVC   NDPTDPTA+2(2),=XL2'FFFF'                                         
         CLI   BCLIAAN,C'Y'         CHECK CLIENT AAN FORMAT                     
         BNE   *+8                                                              
         OI    NDPTCNTL,NDPTAAN                                                 
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    VR100                                                            
*                                                                               
         MVC   TEMPKEY,KEY                                                      
*                                                                               
         OI    DMINBTS,X'08'       READ FOR DELETES                             
         GOTO1 HIGH                                                             
         CLC   KEY(20),TEMPKEY     DOES THIS KEY ALREADY EXIST?                 
         BNE   VR20                NO                                           
*                                                                               
         NI    DMINBTS,X'F7'                                                    
         TM    NDPTCNTL,X'80'      IS IT DELETED?                               
         BZ    VRX                                                              
         NI    NDPTCNTL,X'FF'-X'80'                                             
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'UNTDIR  ',KEY,KEY                      
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(6),TEMPKEY                                                   
         OI    DMINBTS,X'08'       READ FOR DELETES                             
         GOTO1 HIGH                                                             
         B     VR16                                                             
*                                                                               
VR15     GOTO1 SEQ                                                              
*                                                                               
VR16     CLC   KEY(6),TEMPKEY                                                   
         BNE   VRX                                                              
         CLC   KEY(20),TEMPKEY                                                  
         BE    VR15                                                             
         OI    NDPTCNTL,X'80'      MARK FOR DELETION                            
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'UNTDIR  ',KEY,KEY                      
         B     VR15                                                             
*                                                                               
VR20     DS    0H                                                               
         NI    DMINBTS,X'F7'                                                    
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(6),SAVEKEY      GET ORIGINAL KEY                             
         GOTO1 HIGH                                                             
         OI    NDPTCNTL,X'80'      MARK FOR DELETION                            
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'UNTDIR  ',KEY,KEY                      
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(20),TEMPKEY     ADD NEW KEY                                  
         MVC   NDPTDPTA,DPTDPT     ALPHA DAYPART                                
         OC    NDPTDPTA,SPACES                                                  
         MVC   NDPTDPTA+2(2),=XL2'FFFF'                                         
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'UNTDIR  ',KEY,KEY                      
         B     VRX                                                              
*                                                                               
VR100    DS    0H                                                               
         BAS   RE,GETNXTEQ         GET NEXT VALID DAYPART EQUATE                
         MVC   NDPTDPTE,NEXTEQU    DAYPART EQUATE                               
*                                                                               
         MVC   SAVEKEY,KEY                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(20),SAVEKEY                                                  
         BE    RECEXERR            RECORD ALREADY EXISTS                        
*                                                                               
         XC    KEY,KEY                                                          
*                                                                               
         LA    R4,KEY                                                           
         MVC   NDPTDES,DPTDESC     MOVE IN DESCRIPTION TO KEY                   
         OC    NDPTDES,SPACES                                                   
*                                                                               
         MVC   NDPTKEY(2),=XL2'0D07'                                            
         MVC   NDPTAGM,BAGYMD      AGENCY/MEDIA                                 
         MVC   NDPTCLT,BCLT        CLIENT                                       
         MVC   NDPTDPTE,NEXTEQU    DAYPART EQUATE                               
         MVC   NDPTDPTA,DPTDPT     ALPHA DAYPART                                
         OC    NDPTDPTA,SPACES                                                  
         MVC   NDPTDPTA+2(2),=XL2'FFFF'                                         
         CLI   BCLIAAN,C'Y'         CHECK CLIENT AAN FORMAT                     
         BNE   *+8                                                              
         OI    NDPTCNTL,NDPTAAN                                                 
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'UNTDIR  ',KEY,KEY                      
*                                                                               
VRX      DS    0H                                                               
         NI    DMINBTS,X'F7'                                                    
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET NEXT VALID DAYPART EQUATE                                                 
***********************************************************************         
GETNXTEQ NTR1                                                                   
         MVI   NEXTEQU,X'01'                                                    
         ZIC   RF,NEXTEQU                                                       
*                                                                               
         CLC   =C'***',DPTCLT      AGY LEVEL DAYPART?                           
         BNE   *+8                                                              
         AHI   RF,127                                                           
*                                                                               
         STC   RF,NEXTEQU                                                       
*                                                                               
         LA    R4,KEY                                                           
         USING NDPTHDR,R4                                                       
*                                                                               
         MVC   TEMPKEY,KEY                                                      
*                                                                               
GETNEQ10 DS    0H                                                               
         XC    KEY+6(14),KEY+6                                                  
         ZIC   RF,NEXTEQU                                                       
         STC   RF,NDPTDPTE                                                      
*                                                                               
         MVC   SAVEKEY,KEY                                                      
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(6),SAVEKEY                                                   
         BNE   GETNEQ50            FOUND AN AVAILABLE EQUATE                    
*                                                                               
         ZIC   RF,NEXTEQU                                                       
         LA    RF,1(RF)                                                         
         STC   RF,NEXTEQU                                                       
*                                                                               
         GOTO1 SEQ                                                              
         B     GETNEQ10                                                         
*                                                                               
GETNEQ50 DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(20),TEMPKEY                                                  
*                                                                               
GETNEQX  DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY RECORD                                                                
***********************************************************************         
DR       DS    0H                                                               
         LA    R4,KEY                                                           
         USING NDPTHDR,R4                                                       
*                                                                               
         MVC   DPTDPT,NDPTDPTA     DISPLAY DAYPART                              
         OI    DPTDPTH+6,X'80'                                                  
*                                                                               
         MVC   DPTDESC,NDPTDES     DISPLAY DESCRIPTION                          
         OI    DPTDESCH+6,X'80'                                                 
*                                                                               
DRX      DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* LIST RECORDS *                                                                
***********************************************************************         
LR       DS    0H                                                               
         LA    R4,KEY                                                           
         USING NDPTHDR,R4                                                       
*                                                                               
         LA    R2,DPLFRSTH                                                      
*                                                                               
         LA    R5,LISTAR                                                        
         USING PLINED,R5                                                        
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BNE   LR5                                                              
         LA    R5,P                                                             
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         LA    R1,HDRTN                                                         
         ST    R1,HEADHOOK                                                      
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
LR5      MVC   NDPTKEY(2),=XL2'0D07'                                            
         MVC   NDPTAGM,BAGYMD      AGENCY/MEDIA                                 
*                                                                               
         GOTO1 HIGH                                                             
         B     LR20                                                             
*                                                                               
LR10     DS    0H                                                               
         GOTO1 SEQ                                                              
*                                                                               
LR20     DS    0H                                                               
         CLC   KEY(3),SAVEKEY                                                   
         BNE   LRX                                                              
*                                                                               
LR25     OC    BCLT,BCLT           FILTER ON CLIENT?                            
         BZ    LR30                                                             
         CLC   NDPTCLT,BCLT                                                     
         BNE   LR10                                                             
*                                                                               
LR30     OC    TMPDPTA,TMPDPTA     FILTER ON DAYPART?                           
         BZ    LR40                                                             
         CLC   NDPTDPTA,TMPDPTA                                                 
         BL    LR10                                                             
*                                                                               
LR40     DS    0H                  FOUND A MATCH                                
         OC    NDPTCLT,NDPTCLT     AGENCY LEVEL                                 
         BZ    LR50                YES                                          
*                                                                               
LR50     DS    0H                                                               
         OC    NDPTCLT,NDPTCLT                                                  
         BZ    LR55                                                             
         XC    BYTE,BYTE                                                        
         TM    NDPTCNTL,NDPTAAN                                                 
         BZ    *+8                                                              
         MVI   BYTE,C'Y'                                                        
         GOTO1 CLUNPK,DMCB,(BCLIAAN,NDPTCLT),PCLT                               
*                                                                               
LR55     MVC   PDPTA,NDPTDPTA      DISPLAY DAYPART                              
         MVC   PDESC,NDPTDES       DISPLAY DESCRIPTION                          
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BE    LR60                                                             
*                                                                               
         GOTO1 LISTMON                                                          
         B     LR10                                                             
*                                                                               
LR60     DS    0H                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     LR10                                                             
*                                                                               
LRX      B     EXIT                                                             
         EJECT                                                                  
XIT      XIT1  REGS=(R0,R1)                                                     
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* HEADER AND HEAD HOOK ROUTINES                                                 
***********************************************************************         
HEADING  DS    0H                                                               
         SSPEC H1,3,REQUESTOR                                                   
         SSPEC H3,3,C'MEDIA N'                                                  
         SSPEC H1,46,C'NETWORK DAYPART RECORDS'                                 
         SSPEC H2,46,C'-----------------------'                                 
         SSPEC H1,93,AGYNAME                                                    
         SSPEC H2,93,AGYADD                                                     
         SSPEC H3,93,REPORT                                                     
         SSPEC H4,93,RUN                                                        
         SSPEC H5,103,PAGE                                                      
         DC    X'00'                                                            
*                                                                               
HDRTN    NTR1                                                                   
         LA    R2,H8                                                            
         USING PLINED,R2                                                        
*                                                                               
         MVC   PCLT(3),=C'CLT'                                                  
         MVC   PCLT+132(3),=3C'-'                                               
         MVC   PDPTA-1(4),=C'CODE'                                              
         MVC   PDPTA-1+132(4),=4C'-'                                            
         MVC   PDESC(11),=C'DESCRIPTION'                                        
         MVC   PDESC+132(14),=14C'-'                                            
         B     EXIT                                                             
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
INVLACT  DS    0H                                                               
         LA    R2,CONACTH                                                       
         MVI   ERROR,INVACT                                                     
         B     TRAPERR                                                          
*                                                                               
INVLNFND MVI   ERROR,NOTFOUND                                                   
         B     TRAPERR                                                          
*                                                                               
RECEXERR MVI   ERROR,RECEXIST                                                   
         B     TRAPERR                                                          
*                                                                               
AGYERR   DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(40),=C'MUST ADD AGENCY LEVEL DAYPARTS FIRST    '         
         B     MYERR                                                            
*                                                                               
ADDAGYM  DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(40),=C'AGENCY LEVEL DAYPARTS ADDED             '         
         B     MYERR                                                            
*                                                                               
DUPADPT  DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(40),=C'AGENCY LEVEL DAYPART ALREADY EXISTS     '         
         B     MYERR                                                            
*                                                                               
DUPCDPT  DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(40),=C'CLIENT LEVEL DAYPART ALREADY EXISTS     '         
         B     MYERR                                                            
*                                                                               
NODPTLST DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(40),=C'NO MORE DAYPARTS TO LIST                '         
         B     MYERR                                                            
*                                                                               
MYERR    GOTO1 ERREX2                                                           
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
DAYPARTS DS    0H                                                               
         DC    CL2'D ',CL8'DAYTIME'                                             
         DC    CL2'U ',CL8'UNWIRED'                                             
         DC    CL2'F ',CL8'FRINGE'                                              
         DC    CL2'P ',CL8'PRIME'                                               
         DC    CL2'K ',CL8'KIDS'                                                
         DC    CL2'T ',CL8'TEENS'                                               
         DC    CL2'Y ',CL8'YOUTH'                                               
         DC    CL2'S ',CL8'SPORTS'                                              
         DC    CL2'N ',CL8'NEWS'                                                
         DC    CL2'E ',CL8'EARLY'                                               
         DC    CL2'L ',CL8'LATE'                                                
         DC    CL2'C ',CL8'CABLE'                                               
         DC    CL2'O ',CL8'OLYMPICS'                                            
         DC    CL2'R ',CL8'RADIO'                                               
         DC    CL2'H ',CL8'OTHER'                                               
         DC    CL2'J ',CL8'PROMO-ID'                                            
         DC    CL2'X ',CL8'SYND.'                                               
         DC    CL2'I ',CL8'SPECIAL'                                             
         DC    CL2'V ',CL8'OVERNITE'                                            
         DC    CL2'W ',CL8'WKNDPM'                                              
         DC    CL2'M ',CL8'WKNDAM'                                              
         DC    CL2'A ',CL8'ACCESS'                                              
         DC    CL2'B ',CL8'CBLSPORT'                                            
         DC    CL2'Q ',CL8'INTRACTV'                                            
         DC    XL1'FF',CL8' '      END OF TABLE                                 
*                                                                               
         EJECT                                                                  
         PRINT OFF                                                              
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
       ++INCLUDE NESFMA8D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NESFMA9D                                                       
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE NESFMWORKD                                                     
         ORG   SYSSPARE                                                         
*                           *******  T31C08 WORK AREA  *******                  
WORKAREA DS    0CL1                                                             
MYDMWRK  DS    12D                                                              
TEMPFLD  DS    CL50                                                             
*                                                                               
SAVEKEY  DS    XL20                                                             
TEMPKEY  DS    XL20                                                             
*                                                                               
TMPDPTA  DS    XL2                 ALPHA DAYPART                                
TMPDPTE  DS    XL1                 DAYPART MAP EQUATE                           
TMPDESC  DS    XL14                DESCRIPTION                                  
*                                                                               
NEXTEQU  DS    XL1                                                              
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
PLINED   DSECT                                                                  
PCLT     DS    CL3                 CLIENT                                       
         DS    CL2                                                              
PDPTA    DS    XL2                 DAYPART CODE                                 
         DS    CL2                                                              
PDESC    DS    XL14                DESCRIPTION                                  
*                                                                               
CLTRECD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE NEGENDPT                                                       
       ++INCLUDE SPGENAGY                                                       
*                                                                               
         PRINT GEN                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'203NESFM08   11/13/06'                                      
         END                                                                    
