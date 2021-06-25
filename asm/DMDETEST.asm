*          DATA SET DMDETEST   AT LEVEL 002 AS OF 10/28/18                      
*PHASE DMDETSTA                                                                 
*INCLUDE DMDMGRL                                                                
*INCLUDE KHDUMMY                                                                
*INCLUDE DECODE                                                                 
*INCLUDE CARDS                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE SCANNER                                                                
*INCLUDE MDUMPER                                                                
*INCLUDE PRINT                                                                  
         ENTRY SSB                                                              
         ENTRY UTL                                                              
         TITLE 'DEMO testing read/readhi/readsq'                                
**********************************************************************          
* Program allows to put cards in to read demo system to test the                
* various permutations that can occur.                                          
* There are two card sets. First are global settings.                           
*                          Second set are the read commands                     
*                                                                               
* Card 1 table                                                                  
*                                                                               
* DSPACE=                                                                       
* DDSIO=                                                                        
* ACCESS=DANDX or ACCESS=VSAM                                                   
* STATUS=SHOW  or STATUS=HIDE                                                   
*                                                                               
* Card 2 table                                                                  
*                                                                               
* FILLCHR=NULL    X'00'                                                         
*         SPC -   C' '                                                          
*         SPACE - C' '                                                          
*         X'10'   hex value                                                     
*         C'%'    character value                                               
* KEYLEN=18                                                                     
*                                                                               
* OPEN,SYSTEM,LIST                                                              
*      DEMO                                                                     
*             NDEMDIRRNDEMDIRNDEMFILNX                                          
*             NL=DEMFLX                                                         
*                                                                               
* CLOSE,DEMO                                                                    
*                                                                               
* READ,DEMO,DEMDIR,KEY                                                          
* READ,DEMO,DEMFIL,KEY                                                          
* READ,DEMO,DEMFIL,LASTKEY                                                      
* READ,DEMO,DEMFIL,LASTKEY,MINOR KEY                                            
*                                                                               
* READHI,DEMO,DEMDIR,KEY                                                        
* READHI,DEMO,DEMFIL,KEY                                                        
* READHI,DEMO,DEMFIL,LASTKEY                                                    
* READHI,DEMO,DEMFIL,LASTKEY,MINOR KEY                                          
*                                                                               
* READSEQ,DEMO,DEMDIR,KEY                                                       
* READSEQ,DEMO,DEMDIR,KEY,###   where ### is repeat n times                     
* READSEQ,DEMO,DEMFIL,KEY                                                       
* READSEQ,DEMO,DEMFIL,KEY,###   where ### is repeat n times                     
* READSEQ,DEMO,DEMFIL,UNTILEOS  read until end of set                           
*                                                                               
* Note: KEY uses DECODE ie (RTNWNBCT)89F80000000000000000                       
*                          C'RTNWNBCT'                                          
*                                                                               
**********************************************************************          
         PRINT NOGEN                                                            
DMDETEST CSECT                                                                  
         NBASE 0,DMDETST,WORK=A(WORKAREA),R8                                    
                                                                                
         OPEN  (SYSPRINT,OUTPUT)                                                
         BRAS  RE,INIT                                                          
         GOTOR VALPARM,CARDTAB1                                                 
         GOTOR VALPARM,CARDTAB2                                                 
         CLOSE SYSPRINT                                                         
                                                                                
EXIT     XBASE                                                                  
XIT      XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* initialize                                                                    
**********************************************************************          
INIT     NTR1                                                                   
         LA    R3,SCANBLNQ*MAXPRMS                                              
         STORAGE OBTAIN,LOC=31,COND=NO,LENGTH=(R3),ADDR=(R2),BNDRY=PAGE         
         ST    R2,HIBLOCK          High core block of storage                   
*                                                                               
         L     R3,=A(40*K)                                                      
         STORAGE OBTAIN,LOC=24,COND=NO,LENGTH=(R3),ADDR=(R2),BNDRY=PAGE         
         ST    R2,GIBLOCK          Get id block                                 
         J     XIT                                                              
**********************************************************************          
* Validate parmeter cards                                                       
**********************************************************************          
         USING DDNAMED,FILEINFO                                                 
VALPARM  NTR1                                                                   
         ST    R1,ACARDTAB         Pass card table                              
VALPNXT  SAM24                                                                  
         GOTO1 =V(CARDS),PLIST,C,=C'RE00'                                       
         CLC   =C'XX',C                                                         
         JE    VALPEND             Done                                         
         CLC   =C'/*',C                                                         
         JE    VALPEND             Done                                         
         CLC   =C'++DMCOMANDS',C                                                
         JE    VALPEND             Done                                         
         CLI   C,C'*'                                                           
         JE    VALPNXT             Comment                                      
                                                                                
         SAM31                                                                  
         L     R2,HIBLOCK                                                       
         GOTOR =V(SCAN31),DMCB,C,(R2),0,('MAXPRMS',X'20'),(45,0)                
*        GOTOR =V(SCAN31),DMCB,C,(R2),0,'MAXPRMS',SCICARD),0                    
         SR    R0,R0                                                            
         ICM   R0,1,4(R1)          Number of parameters                         
         JNZ   VALP060             None                                         
         CLC   =C'READHI',C                                                     
         JE    VALP020                                                          
         CLC   =C'READ',C                                                       
         JNE   *+2                                                              
*                                                                               
         USING SCANBLKD,R2                                                      
VALP020  GOTOR =V(SCAN31),DMCB,C,(R2),0,(3,SCICARD),0                           
         SR    R0,R0                                                            
         ICM   R0,1,4(R1)                                                       
         JZ    *+2                                                              
         XR    RF,RF                                                            
         LR    R4,R2              Save R2                                       
         LLC   RF,SC1STLEN        P1 length                                     
         AHI   R2,SCANBLNQ        P2                                            
         LLC   RF,SC1STLEN        P2 length                                     
         AHI   R2,SCANBLNQ        P3                                            
         LLC   RF,SC1STLEN        P3 length                                     
         AHI   R2,SCANBLNQ        P4                                            
         AHI   RF,3               For 3 commas                                  
*                                                                               
         LA    R3,C(RF)           P4 should be KEY field                        
         XR    RF,RF                                                            
         CLI   0(R3),C' '                                                       
         JL    *+2                                                              
VALP022  LA    R1,45              45 max                                        
         CLI   0(R3),C','         Search for comma                              
         JE    VALP030                                                          
         AHI   RF,1               Count length                                  
         AHI   R3,1               Bump up in C                                  
         JCT   R1,VALP022                                                       
         J     *+2                                                              
*                                                                               
VALP030  STC   RF,SC1STLEN        Set length to dummy up P4                     
         AHI   R2,SCANBLNQ        P5                                            
         AHI   R3,1               Bump past comma                               
*                                                                               
         LR    RE,R3              Save start of what will be P5                 
         XR    RF,RF                                                            
         LA    R1,8               8 max                                         
VALP032  CLI   0(R3),C' '         Minor key                                     
         JL    *+2                                                              
         JE    VALP036            Done                                          
         AHI   RF,1               Count length                                  
         AHI   R3,1               Bump up in C                                  
         JCT   R1,VALP032                                                       
         J     *+2                                                              
*                                                                               
VALP036  STC   RF,SC1STLEN                                                      
         SHI   RF,1                                                             
         MVC   SC1STFLD(0),0(RE)                                                
         EX    RF,*-6                                                           
*                                                                               
VALP050  LR    R2,R4              Restore R2                                    
         J     VALP060                                                          
                                                                                
         USING PARMD,R3                                                         
VALP060  L     R3,ACARDTAB                                                      
VALP062  CLI   0(R3),EOT                                                        
         JE    ERRINVAL            Invalid parameter card                       
         SR    R5,R5                                                            
         ICM   R5,1,SC1STLEN       Must have a length                           
         JZ    ERRINVAL            Invalid parameter card                       
         CLC   SC1STLEN,PARMLEN                                                 
         JNE   *+10                Check next                                   
         CLC   PARMFLD,SC1STFLD                                                 
         JE    VALP100                                                          
         LA    R3,PARMLNQ(,R3)                                                  
         J     VALP062                                                          
                                                                                
VALP100  TM    PARMTYP,HEX         Want hex input?                              
         JZ    VALP120                                                          
         GOTOR =V(SCAN31),DMCB,C,(R2),0,('MAXPRMS',SCICARD+SCIHEXIN),0          
                                                                                
VALP120  L     R6,PARMVAR          A(Variable)                                  
         L     R9,PARMRTN          A(Routine)                                   
         CLC   SC1STFLD,PARMFLD                                                 
         TM    PARMIND,SGL         Single filed                                 
         JZ    VALP125                                                          
         TM    PARMIND,GOT         Got one                                      
         JO    VALERR2             Duplicate                                    
         OI    PARMIND,GOT         Now we got one                               
*                                                                               
VALP125  TM    PARMTYP,NUM         Looking for a number                         
         JZ    VALP130             No                                           
         TM    SC2NDVAL,SCNUMQ                                                  
         JZ    VALERR3             Not valid number                             
         LTR   R6,R6                                                            
         JZ    VALPRTN                                                          
         MVC   0(4,R6),SC2NDNUM                                                 
         J     VALPRTN             See if routine too                           
*                                                                               
VALP130  TM    PARMTYP,HEX         Looking for a HEX?                           
         JZ    VALP140             No                                           
         TM    SC2NDVAL,SCHEXQ                                                  
         JZ    VALERR4             Not valid hex                                
         LTR   R6,R6                                                            
         JZ    VALPRTN                                                          
         MVC   0(1,R6),SC2NDNUM+3                                               
         CLI   PARMILN,1                                                        
         JE    VALPRTN                                                          
         MVC   0(2,R6),SC2NDNUM+2                                               
         CLI   PARMILN,2                                                        
         JE    VALPRTN                                                          
         MVC   0(3,R6),SC2NDNUM+1                                               
         CLI   PARMILN,3                                                        
         JE    VALPRTN                                                          
         MVC   0(4,R6),SC2NDNUM                                                 
         J     VALPRTN             See if routine too                           
                                                                                
VALP140  TM    PARMTYP,MVC         Move the field into variable                 
         JZ    VALPRTN             No                                           
         LLC   RF,SC2NDLEN                                                      
         TM    PARMTYP,VAR         Fixed or variable                            
         JO    VALP142                                                          
         CLC   SC2NDLEN,PARMILN                                                 
         JL    VALERR5             Invalid input length                         
         JE    VALP144                                                          
         LLC   RF,PARMILN                                                       
         J     VALP144                                                          
                                                                                
VALP142  CLC   SC2NDLEN,PARMILN                                                 
         JH    VALERR5             Invalid input length                         
                                                                                
VALP144  LTR   R6,R6               Make sure you have some place to put         
         JNZ   *+6                                                              
         DC    H'00'               You got to "move it move it"                 
                                                                                
         BCTR  RF,0                                                             
         MVC   0(0,R6),SC1STFLD+SCLFTLNQ      A(SC2NDFLD)                       
         EX    RF,*-6                                                           
         J     VALPRTN             See if routine too                           
                                                                                
VALPRTN  TM    PARMTYP,RTN         Routine                                      
         JZ    VALPADR                                                          
         LTR   R9,R9                                                            
         JNZ   *+6                                                              
         DC    H'00'               You said a routine but it ain't              
         BASR  RE,R9                                                            
         CLI   ERROR#,0                                                         
         JNE   VALERRG                                                          
         J     VALPNXT             Next parameter                               
*                                                                               
VALPADR  TM    PARMTYP,ADR         Address of area                              
         JZ    VALPNXT                                                          
         L     RE,PARMADR                                                       
         A     RE,PARMVAR                                                       
         XR    RF,RF                                                            
         LLC   RF,PARMILN                                                       
         MVC   0(0,RE),SC1STFLD+SCLFTLNQ      A(SC2NDFLD)                       
         EX    RF,*-6                                                           
         J     VALPNXT                                                          
                                                                                
VALPEND  J     XIT                                                              
                                                                                
ERRINVAL MVC   MSG(8),=C'INVALID'                                               
         J     VALERRX                                                          
VALERR1  MVC   MSG(8),=C'P1 ERR '                                               
         J     VALERRX                                                          
VALERR2  MVC   MSG(8),=C'P2 ERR '                                               
         J     VALERRX                                                          
VALERR3  MVC   MSG(8),=C'P3 ERR '                                               
         J     VALERRX                                                          
VALERR4  MVC   MSG(8),=C'P4 ERR '                                               
         J     VALERRX                                                          
VALERR5  MVC   MSG(8),=C'P5 ERR '                                               
         J     VALERRX                                                          
VALERRG  MVC   MSG(8),=C'Error G'                                               
VALERRX  LA    RF,MSG                                                           
         DC    H'00'                                                            
                                                                                
***********************************************************************         
* FILL=character Values can be...                                               
*      NULL                                                                     
*      SPACE                                                                    
*      C' '                                                                     
*      X'00'                                                                    
***********************************************************************         
FILL     NTR1                                                                   
         MVI   FILLCHR,0                                                        
         CLC   =C'NULL',SC1STFLD+SCLFTLNQ      A(SC2NDFLD)                      
         JE    XIT                                                              
         MVI   FILLCHR,C' '                                                     
         CLC   =C'SPC',SC1STFLD+SCLFTLNQ       A(SC2NDFLD)                      
         JE    XIT                                                              
         CLC   =C'SPACE',SC1STFLD+SCLFTLNQ     A(SC2NDFLD)                      
         JE    XIT                                                              
         SAM24                                                                  
         GOTOR =V(DECODE),DMCB,(1,C+5),(0,FILLCHR)                              
         CLI   8(R1),X'FF'                                                      
         JNE   FILLXIT                                                          
         L     R5,8(,R1)                                                        
         NILH  GR5,X'00FF'                                                      
         DC    H'00'                                                            
         MVI   ERROR#,1                                                         
*                                                                               
FILLXIT  CLI   ERROR#,0                                                         
         J     XIT                                                              
                                                                                
***********************************************************************         
* SYSTEM to use                                                                 
***********************************************************************         
         USING DDNAMED,FILEINFO                                                 
SYSIT    NTR1                                                                   
         SAM24                                                                  
         ICM   R2,15,ASYSNAME                                                   
         JNZ   *+8                                                              
         LA    R2,C                                                             
         GOTOR =V(DMDDNAME),DMCB,DMNAME,0(R2)                                   
         L     R2,8(,R1)                                                        
         TM    8(R1),X'10'       System not found                               
         JO    SYSERR10                                                         
*                                                                               
         XC    ASYSNAME,ASYSNAME Clear for next call                            
         MVC   FILEINFO,0(R2)                                                   
         MVC   SYSTEM,=CL8'NET'                                                 
         CLI   DDNASYCH,C'N'     SPOT systems are NET systems                   
         JE    SYSIT20                                                          
         CLI   DDNASECH,C'N'                                                    
         JE    SYSIT20                                                          
         MVC   SYSTEM,=CL8'SPOT'                                                
         CLI   DDNASECH,C'S'                                                    
         JE    SYSIT20                                                          
         MVC   SYSTEM,=CL8'STR'                                                 
         CLI   DDNASECH,C'F'                                                    
         JE    SYSIT20                                                          
         MVC   SYSTEM,=CL8'PRINT'                                               
         CLI   DDNASECH,C'P'                                                    
         JE    SYSIT20                                                          
         MVC   SYSTEM,=CL8'ACCOUNT'                                             
         CLI   DDNASECH,C'A'                                                    
         JE    SYSIT20                                                          
         MVC   SYSTEM,=CL8'DEMO'                                                
         CLI   DDNASECH,C'D'                                                    
         JE    SYSIT20                                                          
         MVC   SYSTEM,=CL8'CONTROL'                                             
         CLI   DDNASECH,C'C'                                                    
         JNE   *+2                                                              
                                                                                
         USING UTLD,RF                                                          
SYSIT20  L     RF,=A(UTL)                                                       
         MVC   TSYS,DDNASENO                                                    
         J     XIT               Exit zero from TM                              
                                                                                
SYSERR10 MVI   ERROR#,10         Exit not zero from TM                          
         J     XIT                                                              
         DROP  RF                                                               
         EJECT                                                                  
**********************************************************************          
* Valid parameters                                                              
**********************************************************************          
CARDTAB1 DS    0A                                                               
         DC    C'DSPACE  ',AL1(6,SGL,1,ADR),A(SSB,SSODSPAC-SSBD)                
         DC    C'DDSIO   ',AL1(5,SGL,8,MVC+VAR),A(0),V(DDSIO)                   
         DC    C'ACCESS  ',AL1(6,SGL,1,ADR),A(SSB,SSODMSTA-SSBD)                
         DC    C'STATUS  ',AL1(6,SGL,1,MVC),A(0,SHOWSTAT)                       
*        DC    C'SYSTEM  ',AL1(6,SGL,8,MVC+VAR),A(0,SYSTEM)                     
*        DC    C'S       ',AL1(1,SGL,0,RTN),A(SYSIT,0)                          
*        DC    C'SYS     ',AL1(3,SGL,0,RTN),A(SYSIT,0)                          
*        DC    C'DDNAME  ',AL1(6,SGL,0,RTN),A(SYSIT,0)                          
         DC    AL1(EOT)                                                         
         EJECT ,                                                                
**********************************************************************          
* Valid parameters                                                              
**********************************************************************          
CARDTAB2 DS    0A                                                               
*        DC    C'ID      ',AL1(2,SGL,8,MVC+VAR),A(0,ID)                         
         DC    C'OPEN    ',AL1(4,0,0,RTN),A(OPEN,0)                             
         DC    C'CLOSE   ',AL1(5,0,0,RTN),A(CLOSE,0)                            
         DC    C'READ    ',AL1(4,0,0,RTN),A(READ,0)                             
         DC    C'READHI  ',AL1(6,0,0,RTN),A(READHI,0)                           
         DC    C'READSEQ ',AL1(7,0,0,RTN),A(READSEQ,0)                          
         DC    C'FILLCHR ',AL1(7,0,0,RTN),A(FILL,0)                             
         DC    C'KEYLEN  ',AL1(6,0,0,NUM),A(0,KEYLEN)                           
*        DC    C'S       ',AL1(1,SGL,0,RTN),A(SYSIT,0)                          
*        DC    C'SYS     ',AL1(3,SGL,0,RTN),A(SYSIT,0)                          
*        DC    C'DDNAME  ',AL1(6,SGL,0,RTN),A(SYSIT,0)                          
         DC    AL1(EOT)                                                         
         EJECT ,                                                                
**********************************************************************          
*                                                                               
**********************************************************************          
*&&DO                                                                           
RUNCHK   NTR1                                                                   
         CLI   DSPACE,C' '                                                      
         BH    RUNCHK10                                                         
         WTO   'MISSING DSPACE CARD'                                            
         B     RUNBAD                                                           
                                                                                
RUNCHK10 LA    R2,VDSPACE          VALIDATE DSPACE                              
         LA    R3,VUPDID           VALIDATE UPDID                               
         LA    R1,L'VDSPACE                                                     
RUNCHK12 CLC   DSPACE,0(R2)                                                     
         BE    RUNCHK14                                                         
         AHI   R2,1                                                             
         AHI   R3,2                                                             
         BRCT  R1,RUNCHK12                                                      
         WTO   'INVALID OR NOT SUPPORTED DSPACE VALUE'                          
         J     RUNBAD                                                           
                                                                                
RUNCHK14 CLC   MCUPDID,SPACES                                                   
         JL    RUNCHK20            NOT REQUIRED                                 
         CLC   MCUPDID,0(R3)       SEE IF MATCH BASED ON DSPACE                 
         JE    RUNCHK20                                                         
         WTO   'UPDID DOES NOT MATCH DSPACE FOR SYSTEM'                         
         J     RUNBAD                                                           
                                                                                
RUNCHK20 CLC   LIST,SPACES                                                      
         JH    RUNCHK22                                                         
         WTO   'MISSING LIST OF FILES TO OPEN'                                  
         J     RUNBAD                                                           
                                                                                
RUNCHK22 LA    RF,MAXFILEQ                                                      
         LA    RE,LIST                                                          
         CLI   0(RE),C'X'                                                       
         JE    RUNCHK25                                                         
RUNCHK24 CLI   0(RE),C'X'          MAKE SURE HAS END                            
         JE    RUNCHK30                                                         
         CLC   1(7,RE),SPACES                                                   
         JNE   RUNCHK26                                                         
RUNCHK25 WTO   'NOT A VALID LIST'                                               
         J     RUNBAD                                                           
                                                                                
RUNCHK26 AHI   RE,8                                                             
         BRCT  RF,RUNCHK24                                                      
         WTO   'NO END OF LIST, 6 MAX'                                          
         J     RUNBAD                                                           
                                                                                
         USING SSBD,RF                                                          
RUNCHK30 CLI   MCMARKER,YES                                                     
         JNE   RUNCHK40                                                         
         L     RF,=A(SSB)                                                       
         OI    SSOMTIND,SSOMRKY                                                 
                                                                                
RUNCHK40 CLC   MCUPDID,SPACES                                                   
         JNH   RUNCHK50                                                         
         L     RF,=V(UPDID)                                                     
         MVC   0(2,RF),MCUPDID     DON'T SET UNLESS HAS VALUE                   
                                                                                
RUNCHK50 CLI   MCWRITE,NO                                                       
         JNE   RUNGOOD                                                          
         L     RF,=A(SSB)                                                       
         OI    SSOMTIND,SSOWRTN                                                 
         DROP  RF                                                               
                                                                                
RUNGOOD  SR    RE,RE                                                            
RUNBAD   LTR   RE,RE                                                            
         J     XIT                                                              
*&&                                                                             
**********************************************************************          
* OPEN,SYSTEM,LIST                                                              
**********************************************************************          
         USING PRTD,P                                                           
OPEN     NTR1                                                                   
         AHI   R2,SCANBLNQ          (P2)  System                                
*                                                                               
OPENP2   CLI   SC1STLEN,0                                                       
         JE    *+2                  Required                                    
         XR    RF,RF                                                            
         MVC   SYSNAME+4(8),SPACES                                              
         LLC   RF,SC1STLEN                                                      
         SHI   RF,1                                                             
         MVC   SYSNAME+4(0),SC1STFLD    SYS=                                    
         EX    RF,*-6                                                           
         LA    RF,SYSNAME                                                       
         ST    RF,ASYSNAME                                                      
         GOTOR SYSIT,SYSNAME                                                    
         JNE   XIT                                                              
*                                                                               
OPENP3   AHI   R2,SCANBLNQ          (P3)  LIST of file                          
         CLC   =C'DEMO',SYSTEM      Specail list in demos                       
         JNE   OPENP3A                                                          
         CLC   =C'NL',SC1STFLD                                                  
         JNE   OPENP3A                                                          
         MVC   LIST(2),SC1STFLD                                                 
         MVI   LIST+2,C'='                                                      
         MVC   LIST+3(12),SC1STFLD+SCLFTLNQ      A(SC2NDFLD)                    
         J     OPENP4                                                           
*                                                                               
OPENP3A  XR    RF,RF                                                            
         LLC   RF,SC1STLEN                                                      
         SHI   RF,1                                                             
         MVC   LIST(0),SC1STFLD                                                 
         EX    RF,*-6                                                           
*                                                                               
OPENP4   DS    0H                                                               
         MVC   PCOMMAND,DMOPEN                                                  
         MVC   PSYSTEM,SYSTEM                                                   
         BRAS  RE,GOPRINT                                                       
         GOTO1 =V(DATAMGR),DMCB,DMOPEN,SYSTEM,LIST,AIO1                         
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
         J     XIT                                                              
**********************************************************************          
* CLOSE,SYSTEM                                                                  
**********************************************************************          
CLOSE    NTR1                                                                   
         AHI   R2,SCANBLNQ          (P2)  System                                
*                                                                               
         CLI   SC1STLEN,0                                                       
         JE    *+2                  Required                                    
         XR    RF,RF                                                            
         MVC   SYSNAME+4(8),SPACES                                              
         LLC   RF,SC1STLEN                                                      
         SHI   RF,1                                                             
         MVC   SYSNAME+4(0),SC1STFLD    SYS=                                    
         EX    RF,*-6                                                           
         LA    RF,SYSNAME                                                       
         ST    RF,ASYSNAME                                                      
         GOTOR SYSIT,SYSNAME                                                    
         JNE   XIT                                                              
*                                                                               
         MVC   PCOMMAND,DMCLOSE                                                 
         MVC   PSYSTEM,SYSTEM                                                   
         BRAS  RE,GOPRINT                                                       
         GOTO1 =V(DATAMGR),DMCB,DMCLOSE,SYSTEM,0,AIO1                           
         CLI   8(R1),0                                                          
         JE    XIT                                                              
         DC    H'00'                                                            
                                                                                
**********************************************************************          
* READ,SYSTEM,DIR,KEY                                                           
* READ,SYSTEM,FILE,KEY                                                          
* READSQ,SYSTEM,DIR,KEY                                                         
* READSQ,SYSTEM,FILE,KEY                                                        
* READHI,SYSTEM,DIR,KEY                                                         
* READHI,SYSTEM,FILE,KEY                                                        
**********************************************************************          
* R3 = current locations in C                                                   
* R7 = repeat factor                                                            
**********************************************************************          
READ     NTR1                                                                   
         MVI   ACTION,C'R'                                                      
         MVC   COMMAND,DMREAD       (P1)  DMREAD                                
         BRAS  RE,SETSYS            (P2)  SETSYS                                
         BRAS  RE,SETFIL            (P3)  SETFIL                                
         BRAS  RE,RDKEY             (P4)  SETKEY                                
         CLI   FILETYPE,FIL                                                     
         JNE   READIT                                                           
         BRAS  RE,MINORKEY          (P5)  MINOR KEY                             
         J     READIT                                                           
                                                                                
READHI   NTR1                                                                   
         MVI   ACTION,C'H'                                                      
         MVC   COMMAND,DMHI         (P1)  DMRDHI                                
         BRAS  RE,SETSYS            (P2)  SETSYS                                
         BRAS  RE,SETFIL            (P3)  SETFIL                                
         BRAS  RE,HIKEY             (P4)  SETKEY                                
         CLI   FILETYPE,FIL                                                     
         JNE   READIT                                                           
         BRAS  RE,MINORKEY          (P5)  MINOR KEY                             
         J     READIT                                                           
                                                                                
READSEQ  NTR1                                                                   
         MVI   ACTION,C'S'                                                      
         MVI   ISEOS,NO                                                         
         MVC   COMMAND,DMSEQ        (P1)  RMRDSQ                                
         BRAS  RE,SETSYS            (P2)  SETSYS                                
         BRAS  RE,SETFIL            (P3)  SETFIL                                
         BRAS  RE,SEQKEY            (P4)  SEQKEY                                
         J     READIT                                                           
                                                                                
***********************************************************************         
* (P2) - Get system information and set UTL+4                                   
***********************************************************************         
SETSYS   ST    RE,SVRE                                                          
         LA    R7,1                 Repeat factor                               
         XR    R3,R3                                                            
         LLC   RF,SC1STLEN                                                      
         LA    R3,1(R3,RF)          Displacement into C plus a comma            
         AHI   R2,SCANBLNQ          (P2)  System                                
*                                                                               
         CLI   SC1STLEN,0                                                       
         JE    *+2                  Required                                    
         MVC   SYSNAME+4(8),SPACES                                              
         LLC   RF,SC1STLEN                                                      
         LA    R3,1(R3,RF)          Displacement into C plus a comma            
         SHI   RF,1                                                             
         MVC   SYSNAME+4(0),SC1STFLD    SYS=                                    
         EX    RF,*-6                                                           
         LA    RF,SYSNAME                                                       
         ST    RF,ASYSNAME                                                      
         GOTOR SYSIT,SYSNAME                                                    
         JNE   *+2                                                              
         L     RE,SVRE                                                          
         BR    RE                                                               
                                                                                
***********************************************************************         
* (P3) - Process which file we want to read  DEMDIR,DEMFIL,PAVDIR, etc          
***********************************************************************         
SETFIL   ST    RE,SVRE                                                          
         AHI   R2,SCANBLNQ                                                      
         LLC   RF,SC1STLEN                                                      
         LA    R3,1(R3,RF)          Displacement into C plus a comma            
         MVC   FILE,SC1STFLD                                                    
         LA    RE,SC1STFLD-3(RF)                                                
         MVI   FILETYPE,DIR                                                     
         CLC   =C'DIR',0(RE)        DIR                                         
         JE    SETFILX                                                          
         MVI   FILETYPE,FIL                                                     
         CLC   =C'MST',0(RE)        FIL                                         
         JE    SETFILX                                                          
         CLC   =C'FIL',0(RE)        FIL                                         
         JNE   *+2                                                              
*                                                                               
SETFILX  L     RE,SVRE                                                          
         BR    RE                                                               
                                                                                
***********************************************************************         
* (P4) - Directory  - KEY                                                       
*        READ       - KEY                                                       
*        READHI     - KEY                                                       
*        READSEQ    - KEY , (P5) number of sequentials                          
***********************************************************************         
***********************************************************************         
* (P4) - File                                                                   
*        READ       - KEY or "LASTKEY" , (P5) Minor key                         
*        READHI     - KEY or "LASTKEY" , (P5) Minor key                         
*        READSEQ    - KEY or "UNTILEOS"  or number of read sequencials          
*                                                                               
***********************************************************************         
                                                                                
***********************************************************************         
* READ SEQUENCIAL                                                               
***********************************************************************         
SEQKEY   ST    RE,SVRE                                                          
         AHI   R2,SCANBLNQ          (P4) KEY                                    
         ICM   RF,1,SC1STLEN                                                    
         JZ    SEQKEY10                                                         
         TM    SC1STVAL,SCNUMQ      Is it a number                              
         JZ    SEQKEY10             Ignore                                      
         L     R7,SC1STNUM          Repeat factor                               
*                                                                               
SEQKEY10 CLI   FILETYPE,DIR                                                     
         JE    SEQKEYX                                                          
         CLC   =C'UNTILEOS',SC1STFLD  Read sequential until EOS                 
         JNE   SEQKEYX                                                          
         MVI   IFEOS,YES                                                        
SEQKEYX  L     RE,SVRE                                                          
         BR    RE                                                               
                                                                                
***********************************************************************         
* READ & READHI                                                                 
***********************************************************************         
RDKEY    DS    0H                                                               
HIKEY    ST    RE,SVRE                                                          
         AHI   R2,SCANBLNQ                                                      
         MVI   KEYWORD,0                                                        
         MVC   KEY,DIRKEY                                                       
         CLC   =C'LASTKEY',SC1STFLD                                             
         JNE   GETKEY               Decode key                                  
         MVI   KEYWORD,LASTKEYQ                                                 
         CLI   FILETYPE,FIL                                                     
         JNE   HIKEYX                                                           
         L     RF,AIO1                                                          
         MVC   0(18,RF),DIRKEY      Reset set IO key                            
         CLI   LASTTYPE,DIR                                                     
         JNE   *+10                                                             
         MVC   18(2,RF),MINOR                                                   
*                                                                               
HIKEYX   L     RE,SVRE                                                          
         BR    RE                                                               
                                                                                
***********************************************************************         
* MINOR KEY FOR FILE READS and READ HIGHS                                       
***********************************************************************         
MINORKEY ST    RE,SVRE                                                          
         XC    MINOR,MINOR          Clear minor key                             
*        CLI   KEYWORD,LASTKYEQ                                                 
*        JNE   MINKEY10                                                         
*        MVC   MINOR,LASTMIN                                                    
*                                                                               
MINKEY10 AHI   R2,SCANBLNQ                                                      
         CLI   SC1STLEN,4                                                       
         JL    MINKEYX                                                          
         CLC   =C'NULL',SC1STFLD                                                
         JE    MINKEYX                                                          
         MVC   WORK,SC1STFLD                                                    
         SAM24                                                                  
         GOTOR =V(DECODE),DMCB,(2,WORK),(FILLCHR,MINOR),0,0                     
         CLI   8(R1),X'FF'                                                      
         JE    *+2                                                              
         L     RF,AIO1                                                          
         MVC   18(2,RF),MINOR                                                   
*                                                                               
MINKEYX  L     RE,SVRE                                                          
         BR    RE                                                               
***********************************************************************         
* SET KEY for read                                                              
* R3 = current location in C (SYSIN CARD)                                       
*                                                                               
***********************************************************************         
GETKEY   LA    R5,C                 Use key passed in                           
         AR    R5,R3                                                            
         MVC   INPUTKEY,0(R5)                                                   
         SAM24                                                                  
         L     R5,KEYLEN                                                        
         GOTOR =V(DECODE),DMCB,((R5),INPUTKEY),(FILLCHR,KEY),0,0                
         CLI   8(R1),X'FF'                                                      
         JE    *+2                                                              
         L     RE,SVRE                                                          
         BR    RE                                                               
***********************************************************************         
*                                                                               
***********************************************************************         
READIT   ST    RE,SVRE                                                          
READLOOP MVC   PCOMMAND,COMMAND                                                 
         MVC   PSYSTEM,SYSTEM                                                   
         MVC   PFILE,FILE                                                       
         MVC   KEY+18(1),STATUS                                                 
         BRAS  RE,PREPRNT                                                       
**********************************************************************          
* Directory reads                                                               
* DIR KEY  is Major key(18), Status(1), Disk address(4)                         
**********************************************************************          
         CLI   FILETYPE,DIR                                                     
         JNE   READ50                                                           
         MVC   PERRMSG,SPACES                                                   
*                                                                               
         GOTOR =V(DATAMGR),DMCB,COMMAND,FILE,KEY,AIO1                           
         CLI   8(R1),0                                                          
         JE    READ20                                                           
*        MVC   PERRMSG,=C'(EOF)'   End of file                                  
*        TM    8(R1),X'80'                                                      
*        JO    READ60                                                           
         MVC   PERRMSG,=C'(RNF)'   Record not    found                          
         TM    8(R1),X'10'                                                      
         JO    READ60                                                           
         MVC   PERRMSG,=C'(URE)'   Un-recoverable error                         
         TM    8(R1),X'40'                                                      
         JO    READ60                                                           
         MVC   PERRMSG,=C'(RMD)'   Record marked deleted                        
         TM    8(R1),X'02'                                                      
         JZ    *+2                                                              
                                                                                
READ20   L     R2,AIO1                                                          
         MVC   STATUS,18(R2)       SAVE STATUS                                  
         TM    STATUS,X'40'        Extended passive?                            
         JZ    *+10                                                             
         MVC   PERRMSG,=C'(XPS)'   Extended passive                             
         MVC   DA,19(R2)           SAVE Disk address                            
         OC    DA,DA               Passive?                                     
         JNZ   *+10                No                                           
         MVC   PERRMSG,=C'(PAS)'   Yes, passive                                 
         MVC   DIRKEY,0(R2)        SAVE Last key                                
         J     READ70                                                           
                                                                                
**********************************************************************          
* File reads                                                                    
* FIL KEY/REC is Major key(18), Minor key(2), Len(2), status(1), rec.           
**********************************************************************          
READ50   CLI   FILETYPE,FIL        File reads                                   
         JNE   *+2                                                              
         MVC   PERRMSG,SPACES                                                   
         L     R2,AIO1                                                          
*        MVC   18(2,R2),MINOR                                                   
         MVC   22(1,R2),STATUS                                                  
         GOTOR =V(DATAMGR),DMCB,COMMAND,FILE,DA,AIO1                            
         CLI   8(R1),0                                                          
         JE    READ60                                                           
         MVI   ISEOS,YES                                                        
         MVC   PERRMSG,=C'(EOS)'   End of record set                            
         TM    8(R1),X'80'                                                      
         JO    READ60                                                           
         MVI   ISEOS,NO                                                         
         MVC   PERRMSG,=C'(RNF)'   Record not found                             
         TM    8(R1),X'10'                                                      
         JO    READ60                                                           
         MVC   PERRMSG,=C'(URE)'   Un-recoverable error                         
         TM    8(R1),X'40'                                                      
         JO    READ60                                                           
         MVC   PERRMSG,=C'(RMD)'   Record marked deleted                        
         TM    8(R1),X'02'                                                      
         JZ    *+2                                                              
*                                                                               
READ60   L     R2,AIO1                                                          
         MVC   GOODKEY,NEWKEY      Save last good key                           
         MVC   NEWKEY,0(R2)                                                     
         MVC   NEWMIN,18(R2)                                                    
*        MVC   STATUS,NEWKEY+22                                                 
         CLI   ISEOS,YES                                                        
         JNE   *+10                                                             
         MVC   NEWKEY,GOODKEY      Restore NEWKEY if EOF                        
*                                                                               
READ70   BRAS  RE,POSTPRNT                                                      
         MVC   LASTTYPE,FILETYPE                                                
         CLI   IFEOS,YES                                                        
         JNE   READ82                                                           
         CLI   ISEOS,NO                                                         
         JE    READLOOP                                                         
         J     XIT                                                              
*                                                                               
READ82   BRCT  R7,READLOOP               Only for a sequential                  
         J     XIT                                                              
**********************************************************************          
* POST PRINT after read                                                         
**********************************************************************          
PREPRNT  NTR1                                                                   
         GOTOR =V(HEXOUT),DMCB,KEY,PKEYHX,KEYLEN,0,0                            
         L     RF,KEYLEN                                                        
         SHI   RF,1                                                             
         MVC   PKEY(0),KEY                                                      
         EX    RF,*-6                                                           
*                                                                               
         CLI   SHOWSTAT,HIDE                                                    
         JE    PREPR20                                                          
         GOTOR =V(HEXOUT),DMCB,STATUS,PSTATHX,1,0,0                             
         MVC   PSTATUS,STATUS                                                   
*                                                                               
PREPR20  CLI   FILETYPE,DIR                                                     
         JE    PREPR80                                                          
         L     R2,AIO1                                                          
         GOTOR =V(HEXOUT),DMCB,18(R2),PMINORHX,2,0,0                            
         MVC   PMINOR,MINOR                                                     
*                                                                               
PREPR80  BRAS  RE,GOPRINT                                                       
         J     XIT                                                              
**********************************************************************          
* POST PRINT after read                                                         
**********************************************************************          
POSTPRNT NTR1                                                                   
         L     R2,AIO1                                                          
         GOTOR =V(HEXOUT),DMCB,AIO1,PKEYHX,KEYLEN,0,0                           
         L     RF,KEYLEN                                                        
         SHI   RF,1                                                             
         MVC   PKEY(0),0(R2)                                                    
         EX    RF,*-6                                                           
*                                                                               
         LA    R5,18(R2)              STATUS in DIR                             
         CLI   FILETYPE,FIL                                                     
         JNE   *+8                                                              
         LA    R5,22(R2)              STATUS in FIL                             
         CLI   SHOWSTAT,HIDE                                                    
         JE    POSTP20                                                          
         GOTOR =V(HEXOUT),DMCB,(R5),PSTATHX,1,0,0                               
         MVC   PSTATUS,0(R5)                                                    
*                                                                               
POSTP20  CLI   FILETYPE,DIR                                                     
         JE    POSTP80                                                          
         GOTOR =V(HEXOUT),DMCB,18(R2),PMINORHX,2,0,0                            
         MVC   PMINOR,18(R2)                                                    
*&&DO                                                                           
         CLI   FILETYPE,DIR                                                     
         JNE   POSTP80                                                          
         AHI   RF,3                                                             
         AR    R4,RF                                                            
         GOTOR =V(HEXOUT),DMCB,DA,PDAHX,8,0,0                                   
*&&                                                                             
POSTP80  BRAS  RE,GOPRINT                                                       
         BRAS  RE,GOPRINT                                                       
         J     XIT                                                              
**********************************************************************          
*                                                                               
**********************************************************************          
READRCV  NTR1                                                                   
         LA    R2,IOKEY                                                         
         MVC   IOKEY,KEY                                                        
         LHI   R0,X'29'                                                         
         XC    DA,DA                                                            
         GOTOR =V(DATAMGR),DMCB,((R0),DMSEQ),ACCRCV,DA,AIO1,0                   
         JNE   *+2                                                              
         XIT1                                                                   
**********************************************************************          
* Print output                                                                  
**********************************************************************          
GOPRINT  NTR1                                                                   
         PUT   SYSPRINT,PCC                                                     
         MVC   P,SPACES                                                         
         J     XIT                                                              
                                                                                
SYSPRINT DCB   DSORG=PS,MACRF=PM,DDNAME=SYSPRINT,RECFM=FBM,LRECL=(133)          
                                                                                
**********************************************************************          
*                                                                               
**********************************************************************          
DIR      EQU   C'D'                                                             
FIL      EQU   C'F'                                                             
ONLY     EQU   C'O'                                                             
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
LASTKEYQ EQU   C'L'                                                             
SHOW     EQU   C'S'                                                             
HIDE     EQU   C'H'                                                             
SCANBLNQ EQU   12+SCLFTLNQ+10                                                   
SCLFTLNQ EQU   45                                                               
*                                                                               
K        EQU   1024                                                             
EOR      EQU   0                                                                
EOT      EQU   X'FF'               End of table                                 
FF       EQU   X'FF'                                                            
MAXPRMS  EQU   15                                                               
MAXFILEQ EQU   6                                                                
ERROR#   DS    X                                                                
KEYWORD  DS    C                                                                
                                                                                
DMWORK   EQU   *                                                                
VDSPACE  DC    C'ACQT'                                                          
VUPDID   DC    C'DMFCFQFT'                                                      
SYSNAME  DC    CL12'SYS='                                                       
LIST     DC    (MAXFILEQ)CL8' '                                                 
         DC    C'X'                                                             
MAXLISTQ EQU   (MAXFILEQ*L'LIST)                                                
ACTION   DS    C                                                                
COMMAND  DC    CL8' '                                                           
FILE     DC    CL8' '                                                           
DMNAME   DC    CL8'DDNAME'                                                      
DMOPEN   DC    CL8'DMOPEN'                                                      
DMCLOSE  DC    CL8'DMCLSE'                                                      
DMKEY    DC    CL8'DMKEY'                                                       
DMREAD   DC    CL8'DMREAD'                                                      
DMWRT    DC    CL8'DMWRT'                                                       
DMSEQ    DC    CL8'DMRSEQ'                                                      
DMHI     DC    CL8'DMRDHI'                                                      
DMGET    DC    CL8'GETREC'                                                      
DMPUT    DC    CL8'PUTREC'                                                      
ACCDIR   DC    CL8'ACCDIR'                                                      
ACCHST   DC    CL8'ACCHST'                                                      
ACCMST   DC    CL8'ACCMST'                                                      
ACCRCV   DC    CL8'ACCRCV'                                                      
CTFILE   DC    CL8'CTFILE'                                                      
DMADD    DC    CL8'ADDREC'                                                      
DADDS    DC    CL8'DADDS'                                                       
DATAMGR  DC    V(DATAMGR)                                                       
                                                                                
ISDATA   DS    0F                                                               
ISATRKS  DS    F                   Available tracks                             
ISUTRKS  DS    F                   Used      tracks                             
ISTTRKS  DS    F                   Total     tracks                             
                                                                                
DADATA   DS    0F                                                               
DAATRKS  DS    F                   Available tracks                             
DAUTRKS  DS    F                   Used      tracks                             
DATTRKS  DS    F                   Total     tracks                             
                                                                                
ACARDTAB DS    A                   Which card table                             
ASYSNAME DS    A                   Used for SYSIT rountine                      
HIBLOCK  DS    A                                                                
GIBLOCK  DS    A                                                                
ADDREC#  DC    F'0'                                                             
BASE#    DC    F'0'                                                             
AIO1     DC    A(IO1)                                                           
AIO2     DC    A(IO2)                                                           
SVRE     DS    A                                                                
DUB      DC    D'0'                                                             
PLIST    DC    6F'0'                                                            
P0       DS    F                                                                
DMCB     DC    6F'0'                                                            
         ORG   DMCB                                                             
P1       DS    F                                                                
P2       DS    F                                                                
P3       DS    F                                                                
P4       DS    F                                                                
P5       DS    F                                                                
P6       DS    F                                                                
                                                                                
DA       DC    F'0'                DISK ADDRESS                                 
FULL     DC    F'0'                                                             
FULL2    DC    F'0'                                                             
WORK     DS    XL36                                                             
FILLCHR  DC    X'00'                                                            
KEYLEN   DC    A(0)                 A type for GOTO macro calls                 
MSG      DS    CL20                                                             
SHOWSTAT DC    AL1(SHOW)            Default to show status                      
*                                                                               
INPUTKEY DS    CL40                                                             
*                                                                               
DIRKEY   DS    XL25                                                             
LASTDIR  DS    XL25                                                             
KEY      DS    XL25                                                             
NEWKEY   DS    XL25                                                             
GOODKEY  DS    XL25                                                             
IOKEY    DS    XL25                                                             
IOWORK   DS    XL96                                                             
*                                                                               
ID       DS    CL8                                                              
DSPACE   DS    C                                                                
FILETYPE DS    C                   D=DIR/F=FILE                                 
LASTTYPE DS    C                   D=DIR/F=FILE                                 
STATUS   DS    X                                                                
MINOR    DS    XL2                                                              
NEWMIN   DS    XL2                                                              
*                                                                               
IFEOS    DS    C                   Y/N IF EOS                                   
ISEOS    DS    C                   Y/N IS EOS                                   
SYSTEM   DS    CL8                                                              
MCUPDID  DS    CL2                                                              
MCMARKER DC    AL1(NO)                                                          
MCWRITE  DC    C' '                YES/NO                                       
FILEINFO DS    XL60                                                             
*                                                                               
C        DC    CL80' '             SYSIN card                                   
*                                                                               
PCC      DC    X'00'                                                            
P        DC    CL132' '                                                         
SPACES   DC    CL132' '                                                         
         LTORG                                                                  
IO1      DS    CL(2*K)                                                          
IO2      DS    CL(2*K)                                                          
WORKAREA DC    5000D'0'                                                         
                                                                                
**********************************************************************          
* SSB and UTL                                                                   
**********************************************************************          
         DC    0D                                                               
         DC    C'**SSB-OFFLINE***'                                              
SSB      DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   (SSOXTND-SSOOFF)+SSB                                             
         DC    X'FF'               SET EXTENDED OFFLINE SSB                     
         ORG   (SSOSTAT2-SSOOFF)+SSB                                            
         DC    AL1(SSOSNRCV)       SET RECOVERY OFF                             
*        ORG   (SSOFWNDX-SSOOFF)+SSB                                            
*        DC    V(FACINDX)          FACWRK INDEX                                 
*        ORG   (SSOFWBUF-SSOOFF)+SSB                                            
*        DC    V(FACBUFF)          FACWRK BUFFER                                
*        DC    A(0)                                                             
         ORG                                                                    
**********************************************************************          
         DC    0D                                                               
         DC    C'**UTL-OFFLINE***'                                              
UTL      DC    XL256'00'                                                        
         EJECT ,                                                                
***********************************************************************         
* DSECTS                                                                        
***********************************************************************         
PARMD    DSECT                                                                  
PARMFLD  DS    CL8                 Input PARM=                                  
PARMLEN  DS    AL1                                                              
PARMIND  DS    X                                                                
GOT      EQU   X'10'               Got one of these                             
SGL      EQU   X'01'               Only single one allowed                      
PARMILN  DS    AL1                                                              
PARMTYP  DS    X                                                                
RTN      EQU   X'80'               Routine                                      
ADR      EQU   X'40'               A(block area)                                
HEX      EQU   X'20'               Hex number input                             
NUM      EQU   X'10'               Number (FIGNUM) Full word value              
MVC      EQU   X'02'               Move character for length n                  
VAR      EQU   X'01'               Variable length                              
PARMADR  DS   0A                                                                
PARMRTN  DS    A                                                                
PARMVAR  DS    A                                                                
PARMLNQ  EQU   *-PARMD                                                          
         EJECT                                                                  
PRTD     DSECT                                                                  
PCOMMAND DS    CL8                                                              
         DS    C                                                                
PSYSTEM  DS    CL8                                                              
         DS    C                                                                
PFILE    DS    CL6                                                              
         DS    C                                                                
PERRMSG  DS    CL5                                                              
         DS    C                                                                
PKEYHX   DS    CL36                                                             
PSTATHX  DS    CL2                                                              
PMINORHX DS    CL4                                                              
         DS    C                                                                
PKEY     DS    CL18                                                             
PSTATUS  DS    C                                                                
PMINOR   DS    CL2                                                              
PRTLENQ  EQU   *-PRTD                                                           
*                                                                               
* SCANBLKD                                                                      
       ++INCLUDE DDSCANBLKD                                                     
                                                                                
DDNAMED  DSECT                                                                  
       ++INCLUDE DMDDNAMED                                                      
*************************                                                       
*        DMGREQUS       *                                                       
*************************                                                       
       ++INCLUDE DMGREQUS                                                       
                                                                                
SSBD     DSECT                                                                  
       ++INCLUDE FASSBOFF                                                       
*UTLD                                                                           
       ++INCLUDE FAUTL                                                          
*                                                                               
*ACGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
         IHAOUXB DSECT=YES                                                      
         IHAASCB LIST=YES                                                       
**********HAASSB LIST=YES                                                       
         IAZJSAB LIST=YES                                                       
         IHAPVT                                                                 
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002DMDETEST  10/28/18'                                      
         END                                                                    
