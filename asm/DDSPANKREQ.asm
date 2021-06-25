*          DATA SET DDSPANKREQ AT LEVEL 005 AS OF 02/13/13                      
*PHASE SPANKRQA                                                                 
*OFFLINE SETC  'Y'                                                              
*INCLUDE DMDMGRL                                                                
*INCLUDE KHDUMMY                                                                
*INCLUDE DECODE                                                                 
*INCLUDE CARDS                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE GETIDS                                                                 
*INCLUDE SCANNER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE STXITER                                                                
*INCLUDE PRINT                                                                  
         ENTRY SSB                                                              
         ENTRY UTL                                                              
         TITLE 'SPANK REQUEST FILE'                                             
         PRINT NOGEN                                                            
SPKREQ   CSECT                                                                  
         NBASE 0,SPKREQ,WORK=A(DCHAIN),R8                                       
         BRAS  RE,VALPARM                                                       
         BRAS  RE,RUNCHK                                                        
         JNE   EXIT                                                             
                                                                                
         OPEN  (SYSPRINT,OUTPUT)                                                
         BRAS  RE,PROCESS                                                       
         BRAS  RE,CLOSE                                                         
         CLOSE SYSPRINT                                                         
                                                                                
EXIT     XBASE                                                                  
XIT      XIT1                                                                   
**********************************************************************          
* Validate parmeter cards                                                       
**********************************************************************          
VALPARM  NTR1                                                                   
         LA    R3,SCBLKLQ*MAXPRMS                                               
         STORAGE OBTAIN,LOC=31,COND=NO,LENGTH=(R3),ADDR=(R2),BNDRY=PAGE         
         ST    R2,HIBLOCK          High core block of storage                   
                                                                                
VALPNXT  SAM24                                                                  
         GOTO1 =V(CARDS),PLIST,C,=C'RE00'                                       
         CLC   =C'XX',C                                                         
         JE    VALPEND             Done                                         
         CLC   =C'/*',C                                                         
         JE    VALPEND             Done                                         
         CLI   C,C'*'                                                           
         JE    VALPNXT             Comment                                      
                                                                                
         SAM31                                                                  
         L     R2,HIBLOCK                                                       
         GOTOR =V(SCAN31),DMCB,C,(R2),0,('MAXPRMS',SCICARD),0                   
         SR    R0,R0                                                            
         ICM   R0,1,4(R1)          Number of parameters                         
         BZ    VALPNXT             None                                         
                                                                                
         USING SCANBLKD,R2                                                      
         USING PARMD,R3                                                         
         LARL  R3,PARMTAB                                                       
VALP020  CLI   0(R3),EOT                                                        
         BE    VALERR1             Invalid parameter card                       
         SR    R5,R5                                                            
         ICM   R5,1,SC1STLEN       Must have a length                           
         BZ    VALERR1             Invalid parameter card                       
         CLC   SC1STLEN,PARMLEN                                                 
         BNE   *+10                Check next                                   
         CLC   PARMFLD,SC1STFLD                                                 
         BE    VALP100                                                          
         LA    R3,PARMLNQ(,R3)                                                  
         B     VALP020                                                          
                                                                                
VALP100  TM    PARMTYP,HEX         Want hex input?                              
         BZ    VALP120                                                          
         GOTOR =V(SCAN31),DMCB,C,(R2),0,('MAXPRMS',SCICARD+SCIHEXIN),0          
                                                                                
VALP120  L     R6,PARMVAR          A(Variable)                                  
         L     R9,PARMRTN          A(Routine)                                   
         TM    PARMIND,SGL         Single filed                                 
         BZ    VALP130                                                          
         TM    PARMIND,GOT         Got one                                      
         BO    VALERR2             Duplicate                                    
         OI    PARMIND,GOT         Now we got one                               
         TM    PARMTYP,NUM         Looking for a number                         
         BZ    VALP130             No                                           
         TM    SC2NDVAL,SCNUMQ                                                  
         BZ    VALERR3             Not valid number                             
         LTR   R6,R6                                                            
         BZ    VALPRTN                                                          
         MVC   0(4,R6),SC2NDNUM                                                 
         B     VALPRTN             See if routine too                           
*                                                                               
VALP130  TM    PARMTYP,HEX         Looking for a HEX?                           
         BZ    VALP140             No                                           
         TM    SC2NDVAL,SCHEXQ                                                  
         BZ    VALERR4             Not valid hex                                
         LTR   R6,R6                                                            
         BZ    VALPRTN                                                          
         MVC   0(1,R6),SC2NDNUM+3                                               
         CLI   PARMILN,1                                                        
         BE    VALPRTN                                                          
         MVC   0(2,R6),SC2NDNUM+2                                               
         CLI   PARMILN,2                                                        
         BE    VALPRTN                                                          
         MVC   0(3,R6),SC2NDNUM+1                                               
         CLI   PARMILN,3                                                        
         BE    VALPRTN                                                          
         MVC   0(4,R6),SC2NDNUM                                                 
         B     VALPRTN             See if routine too                           
                                                                                
VALP140  TM    PARMTYP,MVC         Move the field into variable                 
         BZ    VALPRTN             No                                           
         LLC   RF,SC2NDLEN                                                      
         TM    PARMTYP,VAR         Fixed or variable                            
         BO    VALP142                                                          
         CLC   SC2NDLEN,PARMILN                                                 
         BNE   VALERR5             Invalid input length                         
                                                                                
VALP142  CLC   SC2NDLEN,PARMILN                                                 
         BH    VALERR5             Invalid input length                         
                                                                                
VALP144  LTR   R6,R6               Make sure you have some place to put         
         BNZ   *+6                                                              
         DC    H'00'               You got to "move it move it"                 
                                                                                
         BCTR  RF,0                                                             
         MVC   0(0,R6),SC2NDFLD                                                 
         EX    RF,*-6                                                           
         B     VALPRTN             See if routine too                           
                                                                                
VALPRTN  TM    PARMTYP,RTN         Routine                                      
         BZ    VALPNXT                                                          
         LTR   R9,R9                                                            
         BNZ   *+6                                                              
         DC    H'00'               You said a routine but it ain't              
         BASR  RE,R9                                                            
         CLI   ERROR#,0                                                         
         BNE   VALERRG                                                          
         B     VALPNXT             Next parameter                               
                                                                                
VALPEND  J     XIT                                                              
                                                                                
VALERR1  MVC   MSG(8),=C'Error01'                                               
         B     VALERRX                                                          
VALERR2  MVC   MSG(8),=C'Error02'                                               
         B     VALERRX                                                          
VALERR3  MVC   MSG(8),=C'Error03'                                               
         B     VALERRX                                                          
VALERR4  MVC   MSG(8),=C'Error04'                                               
         B     VALERRX                                                          
VALERR5  MVC   MSG(8),=C'Error05'                                               
         B     VALERRX                                                          
VALERRG  MVC   MSG(8),=C'Error G'                                               
VALERRX  LA    RF,MSG                                                           
         DC    H'00'                                                            
                                                                                
***********************************************************************         
* SYSTEM FROM. REtreive info and request file DTF                               
***********************************************************************         
         USING DDNAMED,R2                                                       
SYSFROM  NTR1                                                                   
         SAM24                                                                  
         MVI   WHERE,C'F'                                                       
         MVC   D(4),=C'SYS='                                                    
         MVC   D+4(5),C+8                                                       
         GOTOR =V(DMDDNAME),DMCB,(X'22',DMNAME),D                               
         L     R2,8(,R1)                                                        
         MVC   FROMINFO,0(R2)                                                   
         J     OPENREQ                                                          
         EJECT                                                                  
***********************************************************************         
* SYSTEM TO. Retreive info and request file DTF                                 
***********************************************************************         
         USING DDNAMED,R2                                                       
SYSTO    NTR1                                                                   
         MVI   WHERE,C'T'                                                       
         SAM24                                                                  
         MVC   D(4),=C'SYS='                                                    
         MVC   D+4(5),C+6                                                       
         GOTOR =V(DMDDNAME),DMCB,(X'22',DMNAME),D                               
         L     R2,8(,R1)                                                        
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
         MVC   TOINFO,0(R2)                                                     
         J     OPENREQ                                                          
         EJECT                                                                  
***********************************************************************         
* OPEN request file, TO and FROM                                                
***********************************************************************         
OPENREQ  LA    RE,REQCOM                                                        
OPENREQ2 CLI   0(RE),X'FF'                                                      
         JE    *+2                                                              
         CLC   0(3,RE),D+4                                                      
         JE    OPENREQ3                                                         
         AHI   RE,REQILNQ                                                       
         J     OPENREQ2                                                         
*                                                                               
OPENREQ3 MVC   REQINFO(REQILNQ),0(RE)                                           
         MVI   WORK,NO                                                          
         CLI   WHERE,C'T'          To or From?                                  
         JNE   OPENREQ4                                                         
         CLI   MCWRITE,YES                                                      
         BNE   *+8                                                              
         MVI   WORK,UPDATE                                                      
*                                                                               
OPENREQ4 CLI   WHERE,C'F'          To or From?                                  
         JNE   OPENREQ5                                                         
         CLI   MCDELETE,YES        Delete moved records?                        
         JNE   OPENREQ5            No                                           
         CLI   MCWRITE,YES         Must have MCWRITE also                       
         BNE   OPENREQ5                                                         
         MVI   WORK,UPDATE                                                      
*                                                                               
OPENREQ5 MVC   WORK+1(7),DDNADDN                                                
         MVI   WORK+8,C'X'                                                      
                                                                                
         USING UTLD,RF                                                          
         L     RF,=V(UTL)                                                       
         MVC   TSYS,DDNASENO                                                    
         DROP  RF                                                               
         GOTOR DATAMGR,DMCB,DMOPEN,REQSYS,WORK,AIO                              
         J     XIT                                                              
*                                                                               
REQCOM   DC    CL3'SPT',CL8'SPOT   ',CL8'REQUEST'                               
         DC    CL3'NET',CL8'NET    ',CL8'REQUEST'                               
         DC    CL3'ACC',CL8'ACCOUNT',CL8'ACCREQ '                               
         DC    CL3'PRT',CL8'PRINT  ',CL8'PREQUES'                               
         DC    CL3'TAL',CL8'TALENT ',CL8'TALREQ '                               
         DC    CL3'STR',CL8'STRAFFI',CL8'TRFREQ '                               
         DC    CL3'TRF',CL8'STRAFFI',CL8'TRFREQ '                               
         DC    CL3'REP',CL8'REP    ',CL8'REPREQ '                               
         DC    X'FF'                                                            
                                                                                
REQINFO  DS    0C                                                               
REQABRV  DS    CL3                                                              
REQSYS   DS    CL8                                                              
REQFINM  DS    CL8                                                              
REQILNQ  EQU   *-REQINFO                                                        
REQRECLQ EQU   (L'RQHHDR+(80*11))                                               
*&&DO                                                                           
REQINHD  DC    XL(L'RQHHDR)'00'    ** REQUEST WORK AREA **                      
REQINCD1 DC    (QDATAMAX)XL(L'QDATA)'00'                                        
REQINCDL EQU   *-REQINCD1                                                       
REQINLN  EQU   *-REQINHD                                                        
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* ALPHAS                                                                        
***********************************************************************         
ALPHAS   NTR1                                                                   
         SAM31                                                                  
         MVC   ALPHATAB,C+7                                                     
         LA    RE,C+7                                                           
         LA    R0,L'ALPHATAB/2                                                  
         XR    RF,RF                                                            
ALPHA2   CLI   0(RE),C' '                                                       
         JE    ALPHA4                                                           
         AHI   RF,1                                                             
         AHI   RE,2                                                             
         JCT   R0,ALPHA2                                                        
*                                                                               
ALPHA4   STC   RF,ALPHA#           Number of alphas to process                  
         LTR   RF,RF                                                            
         JZ    *+2                 None                                         
         J     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* Set DSPACE                                                                    
**********************************************************************          
         USING SSBOFFD,RF                                                       
XDSPACE  MVC   DSPACE,C+7                                                       
         L     RF,=A(SSB)                                                       
         CLI   SSOXTND,FF                                                       
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   SSODSPAC,DSPACE                                                  
         BR    RE                                                               
         DROP  RF                                                               
         EJECT                                                                  
**********************************************************************          
*                                                                               
**********************************************************************          
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
                                                                                
RUNCHK20 CLC   ALPHATAB,SPACES                                                  
         JH    RUNCHK22                                                         
         WTO   'MISSING LIST OF ALPHAS TO PROCESS'                              
         J     RUNBAD                                                           
                                                                                
RUNCHK22 LA    RF,MAXFILEQ                                                      
*&&DO                                                                           
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
*&&                                                                             
         USING SSBOFFD,RF                                                       
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
                                                                                
**********************************************************************          
* PROCESS                                                                       
**********************************************************************          
FROM     USING DDNAMED,FROMINFO                                                 
TO       USING DDNAMED,TOINFO                                                   
         USING UTLD,R5                                                          
         USING REQHDRD,R4                                                       
         USING REQTABD,R8                                                       
                                                                                
PROCESS  NTR1                                                                   
         L     R4,AIO              Area where request is                        
         L     R5,=A(UTL)                                                       
         XC    DA,DA               Initialize                                   
         XR    R3,R3               Total request processed                      
         XR    R6,R6               Total request moved                          
PROC100  MVC   TSYS,FROM.DDNASENO                                               
         GOTOR DATAMGR,DMCB,(X'20',DMSEQ),REQUEST,DA,AIO,0                      
         CLI   8(R1),0                                                          
         JNE   PROC200                                                          
         L     R8,=A(REQBYCO)                                                   
PROC102  CLI   0(R8),0             EOT                                          
         JE    PROC104                                                          
         CLC   RQHAGY,REQALPHA                                                  
         JE    PROC104                                                          
         AHI   R8,REQTLNQ                                                       
         J     PROC102                                                          
*                                                                               
PROC104  MVC   REQALPHA,RQHAGY                                                  
         OC    REQCOUNT,REQCOUNT                                                
         JNZ   *+10                                                             
         ZAP   REQCOUNT,=P'0'                                                   
         AP    REQCOUNT,=P'1'                                                   
*                                                                               
         AHI   R3,1                                                             
         LLC   R0,ALPHA#           Number of alphas to process                  
         LA    RE,ALPHATAB                                                      
PROC106  CLC   RQHAGY,0(RE)        Match alpha?                                 
         JE    PROC110                                                          
         AHI   RE,2                                                             
         JCT   R0,PROC106                                                       
         J     PROC100                                                          
*                                                                               
PROC110  CLC   RQHAGY,=C'99'       Deleted?                                     
         BE    PROC100             Don't move these                             
         AHI   R6,1                                                             
         CLI   MCWRITE,YES                                                      
         BNE   PROC100             Skip actual work                             
         CLI   MCDELETE,YES                                                     
         BNE   PROC130                                                          
         L     R7,AIO                                                           
         AHI   R7,L'RQHHDR                                                      
         MVC   SAVERQ,0(R7)                                                     
         MVC   0(2,R7),=C'99'      Delete from original file                    
*                                                                               
         GOTO1 DATAMGR,DMCB,(X'20',DMWRT),REQUEST,DA,AIO,IOWORK                 
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
*                                                                               
         MVC   0(2,R7),SAVERQ      Restore report                               
*                                                                               
PROC130  MVC   RQHSYS,TO.DDNASENO                                               
         MVC   TSYS,TO.DDNASENO                                                 
         XC    ADDR,ADDR                                                        
         LLC   R2,RQHFLAG          HOB = num of cards-1                         
         SRL   R2,4                                                             
         AHI   R2,2                +1 for -1 and +1 for header                  
         MHI   R2,80               Length of record                             
         GOTOR DATAMGR,DMCB,(X'24',DMADD),REQUEST,ADDR,AIO,0,(R2)               
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
         J     PROC100             Next                                         
*                                                                               
PROC200  CVD   R3,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         MVC   P,SPACES                                                         
         MVC   P(25),=C'Total request processed ='                              
         UNPK  P+26(4),DUB                                                      
         PUT   SYSPRINT,P                                                       
*                                                                               
         CVD   R6,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         MVC   P(25),=C'Total request spanked   ='                              
         UNPK  P+26(4),DUB                                                      
         PUT   SYSPRINT,P                                                       
*                                                                               
         MVC   P,SPACES                                                         
         PUT   SYSPRINT,P                                                       
*                                                                               
         MVC   P(24),=C'Total request for       ='                              
         L     R8,=A(REQBYCO)                                                   
         LA    R3,100                                                           
PROC210  CLI   0(R8),0                                                          
         JE    XIT                                                              
         MVC   P+20(2),REQALPHA                                                 
         OI    REQCOUNT+L'REQCOUNT-1,X'0F'                                      
         UNPK  P+26(4),REQCOUNT                                                 
         PUT   SYSPRINT,P                                                       
         AHI   R8,REQTLNQ                                                       
         JCT   R3,PROC210                                                       
         J     XIT                                                              
**********************************************************************          
* CLOSE files                                                                   
**********************************************************************          
         USING UTLD,R5                                                          
CLOSE    NTR1                                                                   
         L     R4,AIO              Area where request is                        
         L     R5,=A(UTL)                                                       
         MVC   TSYS,FROM.DDNASENO                                               
         GOTOR DATAMGR,DMCB,DMCLOSE,REQSYS,0,0,0                                
         MVC   TSYS,TO.DDNASENO                                                 
         GOTOR DATAMGR,DMCB,DMCLOSE,REQSYS,0,0,0                                
         J     XIT                                                              
         DROP  R5                                                               
         DROP  FROM,TO                                                          
**********************************************************************          
*                                                                               
**********************************************************************          
ONLY     EQU   C'O'                                                             
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
UPDATE   EQU   C'U'                                                             
K        EQU   1024                                                             
EOR      EQU   0                                                                
EOT      EQU   X'FF'               End of table                                 
FF       EQU   X'FF'                                                            
MAXPRMS  EQU   120                                                              
MAXFILEQ EQU   6                                                                
ERROR#   DS    X                                                                
                                                                                
VDSPACE  DC    C'ACQT'                                                          
VUPDID   DC    C'DMFCFQFT'                                                      
LIST     DC    (MAXFILEQ)CL8' '                                                 
         DC    C'X'                                                             
MAXLISTQ EQU   (MAXFILEQ*L'LIST)                                                
COMMAND  DC    CL8' '                                                           
DMNAME   DC    CL8'DDNAME'                                                      
REQUEST  DC    CL8'REQUEST'                                                     
DMOPEN   DC    CL8'DMOPEN'                                                      
DMCLOSE  DC    CL8'DMCLSE'                                                      
DMKEY    DC    CL8'DMKEY'                                                       
DMREAD   DC    CL8'DMREAD'                                                      
DMWRT    DC    CL8'DMWRT'                                                       
DMDIR    DC    CL8'DMRDIR'                                                      
DMSEQ    DC    CL8'DMRSEQ'                                                      
DMHI     DC    CL8'DMRDHI'                                                      
DMGET    DC    CL8'GETREC'                                                      
DMPUT    DC    CL8'PUTREC'                                                      
ACCDIR   DC    CL8'ACCDIR'                                                      
ACCHST   DC    CL8'ACCHST'                                                      
ACCMST   DC    CL8'ACCMST'                                                      
ACCRCV   DC    CL8'ACCRCV'                                                      
CTFILE   DC    CL8'CTFILE'                                                      
DMADD    DC    CL8'DMADD'                                                       
ADDREC   DC    CL8'ADDREC'                                                      
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
                                                                                
HIBLOCK  DS    A                                                                
LOBLOCK  DS    A                                                                
GIBLOCK  DS    A                                                                
ADDREC#  DC    F'0'                                                             
BASE#    DC    F'0'                                                             
AIO      DC    A(IO)                                                            
AREQBYCO DC    A(REQBYCO)          Request by comany                            
SVRE     DS    A                                                                
WORK     DS    CL64                                                             
DUB      DC    D'0'                                                             
PLIST    DC    6F'0'                                                            
DMCB     DC    6F'0'                                                            
         ORG   DMCB                                                             
P1       DS    F                                                                
P2       DS    F                                                                
P3       DS    F                                                                
P4       DS    F                                                                
P5       DS    F                                                                
P6       DS    F                                                                
                                                                                
DA       DC    F'0'                DISK ADDRESS                                 
ADDR     DC    F'0'                New disk address                             
FULL     DC    F'0'                                                             
FULL2    DC    F'0'                                                             
ASID#    DC    H'0'                                                             
FILL     DC    X'40'                                                            
MSG      DS    CL20                                                             
*                                                                               
KEY      DS    XL(ACCRFST-ACCRECD)                                              
SVKEY    DS    XL(ACCRFST-ACCRECD)                                              
IOKEY    DS    XL(ACCRFST-ACCRECD)                                              
IOWORK   DS    XL96                                                             
*                                                                               
ALPHATAB DS    CL70                                                             
ALPHA#   DS    X                                                                
ID       DS    CL8                                                              
DSPACE   DS    C                                                                
MCSYSTEM DS    CL8                                                              
MCUPDID  DS    CL2                                                              
MCMARKER DC    AL1(NO)                                                          
MCWRITE  DC    AL1(NO)             YES/NO                                       
MCDELETE DC    AL1(NO)             YES/NO                                       
WHERE    DC    AL1(0)                                                           
SAVERQ   DS    CL2                                                              
FROMINFO DS    XL60                                                             
TOINFO   DS    XL60                                                             
*                                                                               
C        DC    CL80' '                                                          
D        DC    CL80' '                                                          
*                                                                               
PCC      DC    X'00'                                                            
P        DC    CL132' '                                                         
SPACES   DC    CL132' '                                                         
*                                                                               
                                                                                
DUMPLIST DS    0F                                                               
         DC    A(0)                                                             
         DC    V(DUMMY)                                                         
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
**********************************************************************          
*                                                                               
**********************************************************************          
SYSPRINT DCB   DSORG=PS,MACRF=PM,DDNAME=SYSPRINT,RECFM=FBM,LRECL=(96)           
                                                                                
DCHAIN   DC    5000D'0'                                                         
IO       DS    CL(4*K)                                                          
REQBYCO  DC    (4*K)X'00'                                                       
**********************************************************************          
* Valid parameters                                                              
**********************************************************************          
PARMTAB  DS    0A                                                               
         DC    C'DSPACE  ',AL1(6,SGL,1,RTN),A(XDSPACE,DSPACE)                   
         DC    C'DDSIO   ',AL1(5,SGL,8,MVC+VAR),A(0),V(DDSIO)                   
         DC    C'UPDID   ',AL1(5,SGL,1,MVC),A(0,MCUPDID)                        
         DC    C'WRITE   ',AL1(5,SGL,1,MVC),A(0,MCWRITE)                        
         DC    C'DELETE  ',AL1(6,SGL,1,MVC),A(0,MCDELETE)                       
         DC    C'SYSTO   ',AL1(5,SGL,0,RTN),A(SYSTO,0)                          
         DC    C'SYSFROM ',AL1(7,SGL,0,RTN),A(SYSFROM,0)                        
         DC    C'ALPHAS  ',AL1(6,SGL,0,RTN),A(ALPHAS,0)                         
         DC    AL1(EOT)                                                         
         EJECT ,                                                                
                                                                                
**********************************************************************          
* SSB                                                                           
**********************************************************************          
         DC    0D                                                               
         DC    C'**SSB-OFFLINE***'                                              
SSB      DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   (SSOXTND-SSOOFF)+SSB                                             
         DC    X'FF'               SET EXTENDED OFFLINE SSB                     
         ORG   (SSOSTAT2-SSOOFF)+SSB                                            
         DC    AL1(SSOSNRCV)       SET RECOVERY OFF                             
         ORG                                                                    
*                                                                               
&OFFLINE SETC  'N'                                                              
*                                                                               
**********************************************************************          
         DC    0D                                                               
         DC    C'**UTL-OFFLINE***'                                              
UTL      DC    XL256'00'                                                        
         EJECT ,                                                                
***********************************************************************         
* DSECTS                                                                        
***********************************************************************         
REQTABD  DSECT                                                                  
REQALPHA DS    CL2                                                              
REQRPT   DS    CL2                                                              
REQCOUNT DS    PL8                                                              
REQTLNQ  EQU   *-REQTABD                                                        
*                                                                               
PARMD    DSECT                                                                  
PARMFLD  DS    CL8                 Input PARM=                                  
PARMLEN  DS    AL1                                                              
PARMIND  DS    X                                                                
GOT      EQU   X'10'               Got one of these                             
SGL      EQU   X'01'               Only single one allowed                      
PARMILN  DS    AL1                                                              
PARMTYP  DS    X                                                                
RTN      EQU   X'80'               Routine                                      
HEX      EQU   X'20'               Hex number input                             
NUM      EQU   X'10'               Number (FIGNUM) Full word value              
MVC      EQU   X'02'               Move character for length n                  
VAR      EQU   X'01'               Variable length                              
PARMRTN  DS    A                                                                
PARMVAR  DS    A                                                                
PARMLNQ  EQU   *-PARMD                                                          
                                                                                
NLENQ    EQU   300                                                              
                                                                                
* REQHDRD                                                                       
*                                                                               
REQHDRD  DSECT                                                                  
       ++INCLUDE DMREQHDRA                                                      
* SCANBLKD                                                                      
*                                                                               
       ++INCLUDE DDSCANBLKD                                                     
                                                                                
DDNAMED  DSECT                                                                  
       ++INCLUDE DMDDNAMED                                                      
*************************                                                       
*        DMGREQUS       *                                                       
*************************                                                       
       ++INCLUDE DMGREQUS                                                       
                                                                                
SSBOFFD  DSECT                                                                  
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
         IAZJSAB LIST=YES                                                       
         IHAPVT                                                                 
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005DDSPANKREQ02/13/13'                                      
         END                                                                    
