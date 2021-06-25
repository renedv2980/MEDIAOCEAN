*          DATA SET ACPRO30    AT LEVEL 009 AS OF 12/03/14                      
*PHASE T60B30A                                                                  
         TITLE 'T60B30 - SCHEME RECORD MAINTENANCE'                             
T60B30   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 ,T60B30**                                                        
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9                                                       
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
*                                                                               
         GOTO1 DICTATE,DMCB,C'LU  ',DDIN,DDOUT                                  
         GOTO1 VMODPTRS,DMCB,(X'80',POINTERS)                                   
*                                                                               
         GOTO1 CANWEDEL                                                         
         CLI   MODE,VALKEY                                                      
         BNE   MODE2                                                            
         BAS   RE,VKEY             VALIDATE COMPANY AND CLIENT                  
         B     RESETKEY                                                         
*                                                                               
MODE2    CLI   MODE,VALREC                                                      
         BNE   MODE4                                                            
         BAS   RE,VREC             VALIDATE RECORD                              
         BAS   RE,DREC             DISPLAY NEW RECORD                           
         B     RESETKEY                                                         
*                                                                               
MODE4    CLI   MODE,DISPKEY                                                     
         BNE   MODE6                                                            
         BAS   RE,DKEY             DISPLAY KEY                                  
         BAS   RE,VKEY                                                          
         B     RESETKEY                                                         
*                                                                               
MODE6    CLI   MODE,DISPREC                                                     
         BNE   XIT                                                              
         BAS   RE,DREC             DISPLAY RECORD                               
*                                                                               
RESETKEY MVC   KEY,SAVEKEY         RESTORE KEY                                  
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                          DISPLAY KEY                                *         
***********************************************************************         
*                                                                               
DKEY     NTR1                                                                   
         L     R6,AIO                                                           
         USING ACSHKEY,R6                                                       
         MVC   SCHCODE,ACSHCODE                                                 
         OI    6(R2),X'80'                                                      
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                           VALIDATE KEY                              *         
***********************************************************************         
*                                                                               
VKEY     NTR1                                                                   
         LA    R2,SCHCODEH                                                      
         GOTO1 ANY                                                              
         MVI   ERROR,INVALID                                                    
         CLC   WORK(8),=CL8'ALL'                                                
         BE    ERREXIT                                                          
         LA    R6,KEY                                                           
         USING ACSHKEY,R6                                                       
         XC    ACSHKEY,ACSHKEY                                                  
         MVI   ACSHRTYP,ACSHEQU                                                 
         MVI   ACSHSREC,ACSHSEQU                                                
         MVC   ACSHCUL,CUL                                                      
         MVC   ACSHCODE,WORK                                                    
         MVC   SAVEKEY,KEY         SAVE FOR EXIT                                
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*                        DISPLAY RECORD                               *         
***********************************************************************         
*                                                                               
DREC     NTR1                                                                   
         USING SCHRECD,R6                                                       
         L     R6,AIO                                                           
         OI    SCHBOEH+6,X'80'                                                  
         MVC   SCHBOE,SPACES                                                    
         TM    SCHRSTA,SCHSMCSO                                                 
         BZ    *+10                                                             
         MVC   SCHBOE,AC@ONLY                                                   
         TM    SCHRSTA,SCHSMCSN                                                 
         BZ    *+10                                                             
         MVC   SCHBOE,AC@NO                                                     
         DROP  R6                                                               
*                                                                               
         LA    R2,SCHNAMH                                                       
         MVI   ELCODE,ACSDELQ                                                   
         BAS   RE,GETELIO                                                       
         BNE   DREC02                                                           
         USING ACSDD,R6                                                         
         MVC   8(20,R2),ACSDNAME                                                
         OI    SCHNAMH+6,X'80'                                                  
*                                                                               
DREC02   LA    R2,SCHCOFH          EXTRACT CUTOFF DATE IF IT EXISTS             
         MVI   ELCODE,GDAELQ                                                    
         BAS   RE,GETELIO                                                       
         BNE   DRECX                                                            
         USING GDAELD,R6                                                        
         CLI   GDATYPE,GDASEXPD                                                 
         BNE   DRECX                                                            
         GOTO1 DATCON,DMCB,(1,GDADATE),(17,8(R2))                               
         OI    SCHCOFH+6,X'80'                                                  
*                                                                               
DRECX    B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*                       VALIDATE RECORD                               *         
***********************************************************************         
*                                                                               
VREC     NTR1                                                                   
         USING SCHRECD,R6                                                       
         L     R6,AIO                                                           
         TM    SCHRSTA,SCHSMCSO                                                 
         BNZ   VREC015             ONLY CANNOT BE TAKEN OFF                     
         LA    R2,SCHBOEH                                                       
         NI    SCHRSTA,X'FF'-(SCHSMCSN+SCHSMCSO)                                
         CLC   SCHBOE,SPACES                                                    
         BNH   VREC015                                                          
         XR    R1,R1                                                            
         IC    R1,SCHBOEH+5                                                     
         SHI   R1,1                                                             
         EX    R1,VREC002                                                       
         B     VREC004                                                          
VREC002  CLC   SCHBOE(0),AC@NO                                                  
VREC004  BNE   VREC006                                                          
         OI    SCHRSTA,SCHSMCSN                                                 
         B     VREC015                                                          
VREC006  EX    R1,VREC008                                                       
         B     VREC010                                                          
VREC008  CLC   SCHBOE(0),AC@ONLY                                                
VREC010  BNE   VREC012                                                          
         OI    SCHRSTA,SCHSMCSO                                                 
         B     VREC015                                                          
VREC012  MVI   ERROR,INVALID                                                    
         B     ERREXIT                                                          
         DROP  R6                                                               
*                                                                               
VREC015  LA    R2,SCHNAMH                                                       
         GOTO1 ANY                                                              
         MVI   ELCODE,ACSDELQ      DELETE OLD DEFINITION ELEMENT, IF            
         BAS   RE,GETELIO           THERE, AND ADD NEW ONE                      
         BNE   VREC020                                                          
         GOTO1 REMELEM                                                          
*                                                                               
         USING ACSDD,R6                                                         
VREC020  LA    R6,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   ACSDEL,ACSDELQ                                                   
         MVI   ACSDLEN,X'1E'                                                    
         MVC   ACSDNAME,WORK                                                    
         GOTO1 ADDELEM                                                          
*                                                                               
         USING GDAELD,R6                                                        
         LA    R2,SCHCOFH                                                       
         MVI   ELCODE,GDAELQ                                                    
         BAS   RE,GETELIO                                                       
         BNE   VREC021                                                          
         CLI   GDATYPE,GDASEXPD                                                 
         BNE   VREC021                                                          
         GOTO1 REMELEM                                                          
*                                                                               
VREC021  CLC   SCHCOF,SPACES                                                    
         BNH   VREC024                                                          
         LA    R6,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   GDAEL,GDAELQ                                                     
         MVI   GDALN,GDALNQ                                                     
         MVI   GDATYPE,GDASEXPD                                                 
         GOTO1 DATVAL,DMCB,(0,SCHCOF),(0,WORK)                                  
         OC    DMCB,DMCB                                                        
         BNZ   VREC022                                                          
         LA    R2,SCHCOFH           DATE INVALID WARN USER                      
         MVI   ERROR,INVDATE                                                    
         B     ERREXIT                                                          
*                                                                               
VREC022  GOTO1 DATCON,DMCB,(0,WORK),(1,GDADATE)                                 
         MVI   ERROR,DATNOFUT                                                   
         CLC   GDADATE,TODAYP                                                   
         BNH   ERREXIT                                                          
         GOTO1 ADDELEM                                                          
*                                                                               
VREC024  CLI   ACTNUM,ACTADD                                                    
         BNE   VRECX                                                            
         DROP  R6                                                               
*                                                                               
         USING ACSCD,R6                                                         
VREC025  LA    R6,ELEMENT                                                       
         XC    ELEMENT,ELEMENT     IF 'ADD'', ADD CATEGORY SEQUENCE             
         MVI   ACSCEL,ACSCELQ       ELEMENT WITH "SLUSH" ACCOUNT                
         MVC   ACSCLEN,=YL1(1*2+2)                                              
         MVC   ACSCAT(2),=X'FFFF'                                               
*                                                                               
         GOTO1 ADDELEM                                                          
*                                                                               
         MVC   AIO,AIO2            SWAP IO AREA FOR CATEGORY RECORD             
*                                                                               
         LA    R6,KEY              ADD "SLUSH" CATEGORY RECORD ALSO             
         USING ACCTKEY,R6                                                       
         XC    ACCTKEY,ACCTKEY                                                  
         MVI   ACCTRTYP,ACCTEQU                                                 
         MVI   ACCTSREC,ACCTSEQU                                                
         MVC   ACCTCUL,CUL                                                      
         LA    R2,SCHCODEH                                                      
         GOTO1 ANY                                                              
         MVC   ACCTSCH,WORK                                                     
         MVC   ACCTCODE(2),=X'FFFF'                                             
         USING ACKEYD,R6                                                        
         OI    DMINBTS,X'08'       READ DELETED RECORDS AS WELL                 
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(ACCTCODE-ACCTKEY+2),KEY                                  
         BE    VREC040                                                          
*                                                                               
         NI    DMINBTS,X'F7'                                                    
         L     RE,AIO                                                           
         L     RF,SIZEIO                                                        
         XCEF                                                                   
         L     R6,AIO                                                           
         MVC   ACCTKEY,KEYSAVE                                                  
         MVC   ACLENGTH,=Y(ACRECORD-ACKEYD+1)                                   
         MVC   KEY(L'ACCTKEY),ACCTKEY                                           
         GOTO1 ADD                                                              
*                                                                               
         LA    R6,ELEMENT          ADD CATEGORY DESCRIPTION ELEMENT TOO         
         USING ACCDD,R6                                                         
         XC    ELEMENT,ELEMENT                                                  
         MVI   ACCDEL,ACCDELQ                                                   
         MVC   ACCDLEN,=YL1(ACCDINST-ACCDD)                                     
         MVC   ACCDNAME,SPACES                                                  
         MVC   ACCDNAME(13),AC@MISC                                             
         MVI   ACCDTYPE,ACCDWORK   TYPE=WORKCODE LIST                           
         GOTO1 ADDELEM                                                          
*                                                                               
         GOTO1 PERSIN              UPDATE PERSONAL ELEMENT IN CATEGORY          
         GOTO1 WRITE               WRITE BACK CATEGORY RECORD                   
*                                                                               
*                                  ADD PASSIVE POINTERS FOR CATEGORY            
         GOTO1 VSAVPTRS,DMCB,(X'80',0),CATPTRS                                  
         GOTO1 VCHGPTRS,DMCB,(X'80',AIO),CATPTRS                                
*                                                                               
VREC040  MVC   AIO,AIO1            GET IO AREA WITH SCHEME RECORD NOW           
*                                                                               
VRECX    GOTO1 PERSIN              UPDATE PERSONAL ELEMENT IN SCHEME            
*                                                                               
XIT      XIT1                                                                   
*                                                                               
ERRMSG   ST    RE,FULL                                                          
         SR    RF,RF                                                            
         ICM   RF,4,GETMSYS                                                     
         ST    R2,ACURFORC                                                      
         GOTO1 GETTXT,DMCB,(R0),(0,CONHEADH),(C'I',0),0,0,(RF)                  
         OI    GENSTAT2,USMYOK                                                  
         L     RE,FULL                                                          
         BR    RE                                                               
*                                                                               
ERREXIT  GOTO1 VERRCUR                                                          
*                                                                               
GETELIO  L     R6,AIO                                                           
         GETEL (R6),DATADISP,ELCODE                                             
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
DDIN     DS    0C                                                               
         DCDDL AC#MISC,13                                                       
         DCDDL AC#NO,4                                                          
         DCDDL AC#ONLY,4                                                        
         DC    X'00'                                                            
         EJECT                                                                  
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
*  DDSPOOLD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
*  DDSPLWORKD                                                                   
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
*  ACGENBOTH                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
*  ACGENFILE                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
*  ACPROWORKD                                                                   
         PRINT OFF                                                              
       ++INCLUDE ACPROWORKD                                                     
         PRINT ON                                                               
*  ACMSGEQUS                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACMSGEQUS                                                      
         PRINT ON                                                               
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE ACPROC0D                                                       
SAVEKEY  DS    CL42                HOLD KEY FOR EXIT                            
DDOUT    DS    0C                                                               
         DSDDL PRINT=YES                                                        
POINTERS DS    XL(8*54+1)          PASSIVE POINTER BLOCK                        
CATPTRS  DS    XL(8*54+1)          CATEGORY PASSIVE POINTER BLOCK               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009ACPRO30   12/03/14'                                      
         END                                                                    
