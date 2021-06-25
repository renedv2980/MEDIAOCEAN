*          DATA SET CTINF00A   AT LEVEL 046 AS OF 05/01/02                      
*PHASE TA0500A,*                                                                
*INCLUDE CHOPPER                                                                
*INCLUDE SQUASHER                                                               
         TITLE 'TA0500 - CONTROL FILE INFO FACILITY'                            
INFOMOD  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WRKX-WRKD,**INFO**,R9,RR=RE                                      
         USING WRKD,RC                                                          
         ST    RE,RELO                                                          
         MVC   PARMS,0(R1)                                                      
         L     RA,ATWA                                                          
         USING CTINFD,RA                                                        
         LA    R7,IO                                                            
         L     RE,AUTL                                                          
         USING UTLD,RE                                                          
         MVC   USERID,TUSER                                                     
         MVC   TERMINAL,TNUM                                                    
         MVI   DDS,0               SET DDS TERMINAL FLAG                        
         TM    TSTAT,TSTATDDS                                                   
         BZ    *+8                                                              
         OI    DDS,1               SET THIS IS A DDS TERMINAL                   
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(79),SPACES                                              
         MVC   P,SPACES                                                         
         L     R1,ACOMFACS                                                      
         USING COMFACSD,R1                                                      
         MVC   DATAMGR,CDATAMGR                                                 
         MVC   DATCON,CDATCON                                                   
         MVC   GETMSG,CGETMSG                                                   
         MVC   SCANNER,CSCANNER                                                 
         MVC   HEXOUT,CHEXOUT                                                   
         L     RF,CGETFACT                                                      
         DROP  R1                                                               
         GOTO1 (RF),DMCB,0         GET A(SYSTEM LIST) FROM GETFACT              
         L     R1,0(R1)                                                         
         L     R1,FASYSLST-FACTSD(R1)                                           
         LA    R1,SYSLLEN+6(R1)                                                 
         ST    R1,ASYSLST                                                       
         L     RE,=V(CHOPPER)                                                   
         A     RE,RELO                                                          
         ST    RE,CHOPPER                                                       
         L     RE,=V(SQUASHER)                                                  
         A     RE,RELO                                                          
         ST    RE,SQUASHER                                                      
         B     VALREC                                                           
         EJECT                                                                  
* VALIDATE RECORD TYPE                                                          
*                                                                               
VALREC   LA    R2,INFRECH                                                       
         MVI   ERROR,1                                                          
         CLI   5(R2),0                                                          
         BE    ERREX                                                            
         SR    R3,R3                                                            
         IC    R3,5(R2)                                                         
         CH    R3,=H'3'                                                         
         BL    *+8                                                              
         LA    R3,3                                                             
         BCTR  R3,0                                                             
         LA    R4,RECTAB                                                        
         SPACE 1                                                                
VR2      CLI   0(R4),X'FF'                                                      
         BNE   *+12                                                             
         MVI   ERROR,10                                                         
         B     ERREX                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),0(R4)                                                    
         BE    *+12                                                             
VR3      LA    R4,L'RECTAB(R4)                                                  
         B     VR2                                                              
         TM    DDS,1               TEST IF A DDS TERMINAL                       
         BNZ   VR4                                                              
         TM    10(R4),X'80'        TEST IF A USER RECORD TYPE                   
         BZ    VR3                                                              
         TM    12(RA),X'80'        TEST USER TERMINAL AUTHORIZED                
         BZ    VR3                                                              
VR4      MVC   8(8,R2),0(R4)                                                    
         OI    6(R2),X'80'                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(1),9(R4)                                                     
         SR    RE,RE                                                            
         ICM   RE,7,11(R4)                                                      
         A     RE,RELO                                                          
         ST    RE,ARTN                                                          
         EJECT                                                                  
* VALIDATE KEYS                                                                 
*                                                                               
VALKEY   LA    R2,INFKEYH                                                       
         ZIC   R3,5(R2)                                                         
         CLC   8(4,R2),=C'NEXT'                                                 
         BNE   VK2                                                              
         MVC   INFKEY,SPACES                                                    
         OI    INFKEYH+6,X'80'                                                  
         CLC   LASTCODE,INFREC                                                  
         BE    *+10                                                             
         SR    R3,R3                                                            
         B     VK2                                                              
         MVC   KEY,LASTKEY                                                      
         B     VALFILT                                                          
         SPACE 1                                                                
VK2      MVC   LASTCODE,INFREC                                                  
         MVC   WORK,SPACES                                                      
         LTR   R3,R3                                                            
         BZ    VK3                                                              
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),8(R2)                                                    
         SPACE 1                                                                
VK3      IF    KEY,=,C'E',VK4                                                   
         IF    KEY,=,C'I',VK6                                                   
         IF    KEY,=,C'P',VK8                                                   
         IF    KEY,=,C'T',VK10                                                  
         IF    KEY,=,C'U',VK12                                                  
         IF    KEY,=,C'X',VK14                                                  
         IF    KEY,=,C'Y',VK16                                                  
         IF    KEY,=,C'0',VK18                                                  
         IF    KEY,=,C'A',VK20                                                  
         MVC   KEY+14(10),WORK                                                  
         B     VALFILT                                                          
         SPACE 1                                                                
VK4      LTR   R3,R3                                                            
         BZ    VALFILT                                                          
         BCTR  R3,0                                                             
         L     R1,ASYSLST                                                       
         USING SYSLSTD,R1                                                       
         TM    INFKEYH+4,X'08'     TEST NUMERIC KEY                             
         BZ    VK5                                                              
         EX    R3,*+8                                                           
         B     *+10                                                             
         PACK  DUB,INFKEY(0)                                                    
         CVB   R1,DUB                                                           
         STC   R1,KEY+23                                                        
         B     VALFILT                                                          
*                                  SEARCH TABLE FOR SYSTEM                      
VK5      CLI   SYSLNUM,0                                                        
         BE    VALFILT                                                          
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   SYSLNAME(0),WORK                                                 
         BE    *+12                                                             
         LA    R1,SYSLLEN(R1)                                                   
         B     VK5                                                              
         MVC   KEY+23(1),SYSLNUM                                                
         B     VALFILT                                                          
         DROP  R1                                                               
         SPACE 1                                                                
VK6      MVC   KEY+15(10),WORK                                                  
         B     VALFILT                                                          
         SPACE 1                                                                
VK8      MVC   KEY+19(5),WORK                                                   
         B     VALFILT                                                          
         SPACE 1                                                                
VK10     MVC   KEY+7(8),WORK                                                    
         B     VALFILT                                                          
         SPACE 1                                                                
VK12     MVC   KEY+11(1),WORK                                                   
         MVC   KEY+13(2),WORK+1                                                 
         CLI   5(R2),3             TEST 3 CHARCTER INPUT                        
         BNH   VALFILT                                                          
         MVC   KEY+12(3),WORK+1                                                 
         B     VALFILT                                                          
         SPACE 1                                                                
VK14     MVC   KEY+12(5),WORK                                                   
         B     VALFILT                                                          
         SPACE 1                                                                
VK16     MVC   KEY+20(3),WORK                                                   
         B     VALFILT                                                          
         SPACE 1                                                                
VK18     MVC   KEY+1(2),14(RA)     ALPHA AGY FROM TWA                           
         CLI   INFREC,C'N'         TEST ACCESS BY NAME                          
         BNE   VK18A                                                            
         CLC   WORK,SPACES         TEST KEY VALUE PRESENT                       
         BNE   *+12                                                             
         MVI   KEY+5,X'41'         NO - SET LOW KEY VALUE                       
         B     VALFILT                                                          
         MVC   KEY+3(10),WORK                                                   
         B     VALFILT                                                          
*                                                                               
VK18A    MVC   KEY+15(10),WORK     MUST BE ACCESS BY CODE                       
         B     VALFILT                                                          
VK20     MVC   KEY+16(2),USERID                                                 
         B     VALFILT                                                          
         EJECT                                                                  
* VALIDATE FILTERS                                                              
*                                                                               
VALFILT  XC    FILTAREA,FILTAREA                                                
         MVI   FILTPROF,C' '                                                    
         LA    R2,INFFILTH                                                      
         XC    BLOCK,BLOCK                                                      
         CLI   5(R2),0                                                          
         BE    VFX                                                              
         GOTO1 SCANNER,DMCB,(R2),(5,BLOCK)                                      
         MVI   ERROR,2                                                          
         SR    R3,R3                                                            
         ICM   R3,1,DMCB+4                                                      
         BZ    ERREX                                                            
         LA    R4,BLOCK                                                         
         SPACE 1                                                                
VF2      CLC   =C'ID',12(R4)                                                    
         BNE   VF4                                                              
         MVC   FILTID,22(R4)                                                    
         MVC   IO2(L'KEY),KEY                                                   
         XC    KEY,KEY                                                          
         MVI   KEY,C'I'                                                         
         MVC   KEY+15(10),FILTID                                                
         BAS   RE,HIGH                                                          
         LR    R4,R7                                                            
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         MVC   FILTIDNO,2(R4)                                                   
         MVC   KEY,IO2                                                          
         B     VFNEXT                                                           
         SPACE 1                                                                
VF4      CLC   =C'PASS',12(R4)                                                  
         BNE   VF6                                                              
         MVC   FILTPASS,22(R4)                                                  
         B     VFNEXT                                                           
         SPACE 1                                                                
VF6      CLC   =C'PROF',12(R4)                                                  
         BNE   VF8                                                              
         MVC   FILTPROF,22(R4)                                                  
         B     VFNEXT                                                           
         SPACE 1                                                                
VF8      CLC   =C'OUTPUT',12(R4)                                                
         BNE   VF10                                                             
         MVC   FILTOUT,22(R4)                                                   
         B     VFNEXT                                                           
         SPACE 1                                                                
VF10     CLC   =C'AGY',12(R4)                                                   
         BNE   VF12                                                             
         ZIC   R1,1(R4)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FILTAGY+1(0),22(R4)                                              
         STC   R1,FILTAGY                                                       
         B     VFNEXT                                                           
         SPACE 1                                                                
VF12     DS    0H                                                               
         SPACE 1                                                                
VFNEXT   LA    R4,32(R4)                                                        
         BCT   R3,VF2                                                           
VFX      BAS   RE,PAGECLR          GO TO SPECIFIC ROUTINE                       
         L     RE,ARTN                                                          
         BR    RE                                                               
         EJECT                                                                  
* ATTENTION RECORDS (ID#3)                                                      
*                                                                               
         USING CTIREC,R7                                                        
ATT      MVC   PAGE(33),=C'ID      LIST OF ATTENTION TYPES'                     
         BAS   RE,HIGH                                                          
         B     ATT4                                                             
         SPACE 1                                                                
ATT2     BAS   RE,SEQ                                                           
         SPACE 1                                                                
ATT4     BAS   RE,FILTER                                                        
         BNE   ATT2                                                             
         MVC   P,SPACES                                                         
         MVI   ELCODE,X'31'                                                     
         MVC   P(6),CTIKID                                                      
         LA    R3,P+10                                                          
         USING CTATTND,R4                                                       
         LR    R4,R7                                                            
         BAS   RE,GETEL                                                         
         BNE   ATT2                                                             
         B     ATT8                                                             
         SPACE 1                                                                
ATT6     BAS   RE,NEXTEL                                                        
         BE    ATT8                                                             
         GOTO1 SQUASHER,DMCB,P+10,70                                            
         BAS   RE,ADD                                                           
         B     ATT2                                                             
         SPACE 1                                                                
ATT8     MVC   0(3,R3),CTATTTYP                                                 
         LA    R3,4(R3)                                                         
         B     ATT6                                                             
         EJECT                                                                  
* DESTINATION RECORDS (ID#2)                                                    
*                                                                               
         USING CTIREC,R7                                                        
DEST     MVC   PAGE(24),=C'ID         LOGO1   LOGO2'                            
         MVC   PAGE+27(23),=C'POWER UNIT  DESTINATION'                          
         MVC   PAGE+107(04),=C'CODE'                                            
         OC    CTIKID,SPACES                                                    
         BAS   RE,HIGH                                                          
         B     DEST4                                                            
         SPACE 1                                                                
DEST2    BAS   RE,SEQ                                                           
         SPACE 1                                                                
DEST4    BAS   RE,FILTER                                                        
         BNE   DEST2                                                            
         MVC   P,SPACES                                                         
         MVC   P(10),CTIKID                                                     
         MVI   ELCODE,X'30'                                                     
         LR    R4,R7                                                            
         BAS   RE,GETEL                                                         
         BNE   DEST6                                                            
         USING CTDSTD,R4                                                        
         MVC   P+11(7),CTDSTLG1                                                 
         MVC   P+19(7),CTDSTLG2                                                 
         MVC   P+27(4),CTDSTPOW                                                 
         MVC   P+39(33),CTDSTNAM                                                
         SPACE 1                                                                
DEST6    LR    R4,R7                                                            
         MVI   ELCODE,X'4C'                                                     
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         USING CTSHPD,R4                                                        
         MVC   P+34(2),CTSHPINS                                                 
         BAS   RE,ADD                                                           
         B     DEST2                                                            
         EJECT                                                                  
* ERROR MESSAGES                                                                
*                                                                               
ER       MVC   PAGE(31),=C'SYSTEM   ERROR    ERROR MESSAGE'                     
         MVC   PAGE+89(6),=C'NUMBER'                                            
         BAS   RE,HIGH                                                          
         B     ER4                                                              
         SPACE 1                                                                
ER2      BAS   RE,SEQ                                                           
         SPACE 1                                                                
ER4      BAS   RE,FILTER                                                        
         BNE   ER2                                                              
         USING CTEREC,R7                                                        
         MVC   P,SPACES                                                         
         MVC   P(3),=C'ALL'                                                     
         CLI   CTEKSYS,0                                                        
         BE    ER6                                                              
*                                  FIND SYSTEM ENTRY IN SYSLST                  
         L     R1,ASYSLST                                                       
         USING SYSLSTD,R1                                                       
ER5      CLI   SYSLNUM,0                                                        
         BNE   *+14                                                             
         MVC   P(7),=CL7'UNKNOWN'                                               
         B     ER6                                                              
         CLC   CTEKSYS,SYSLNUM                                                  
         BE    *+12                                                             
         LA    R1,SYSLLEN(R1)                                                   
         B     ER5                                                              
         MVC   P(7),SYSLNAME                                                    
         DROP  R1                                                               
         SPACE 1                                                                
ER6      LR    R4,R7                                                            
         EDIT  (1,CTEKNUM),(3,P+10)                                             
         USING CTDSCD,R4                                                        
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         SR    R3,R3                                                            
         IC    R3,CTDSCLEN                                                      
         SH    R3,=H'3'                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   P+18(0),CTDSC                                                    
         BAS   RE,ADD                                                           
         B     ER2                                                              
         EJECT                                                                  
* ID RECORDS                                                                    
*                                                                               
ID       MVC   PAGE(39),=C'ID CODE    ID NUM.   SYSTEMS AUTHORIZED'             
         MVC   PAGE+52(07),=C'ID LIST'                                          
         USING CTIREC,R7                                                        
         OC    CTIKID,SPACES                                                    
         BAS   RE,HIGH                                                          
         B     ID4                                                              
         SPACE 1                                                                
ID2      BAS   RE,SEQ                                                           
         SPACE 1                                                                
ID4      BAS   RE,FILTER                                                        
         BNE   ID2                                                              
         MVC   P,SPACES                                                         
         MVC   P(10),CTIKID                                                     
         LR    R4,R7                                                            
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         USING CTDSCD,R4                                                        
         EDIT  (2,CTDSC),(4,P+11)                                               
         BAS   RE,AUTH                                                          
         GOTO1 CHOPPER,DMCB,(64,WORK),(30,P+21),1                               
         BAS   RE,IDLIST                                                        
         BAS   RE,ADD                                                           
         B     ID2                                                              
         EJECT                                                                  
* JCL AND LIBRARY                                                               
*                                                                               
LIB      DS    0H                                                               
FILES    DS    0H                                                               
JCL      MVC   PAGE(29),=C'BOOK NAME    BOOK DESCRIPTION'                       
         USING CTJREC,R7                                                        
         BAS   RE,HIGH                                                          
         B     JCL4                                                             
         SPACE 1                                                                
JCL2     MVI   KEY+24,X'FF'                                                     
         BAS   RE,HIGH                                                          
         SPACE 1                                                                
JCL4     BAS   RE,FILTER                                                        
         BNE   JCL2                                                             
         MVC   P,SPACES                                                         
         CLI   CTJKSUB,0                                                        
         BNE   JCL2                                                             
         MVC   P(10),CTJKID                                                     
         LR    R4,R7                                                            
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BNE   JCL6                                                             
         USING CTDSCD,R4                                                        
         SR    R3,R3                                                            
         IC    R3,CTDSCLEN                                                      
         SH    R3,=H'3'                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   P+13(0),CTDSC                                                    
JCL6     BAS   RE,ADD                                                           
         B     JCL2                                                             
         EJECT                                                                  
* KWX ADDRESSEE RECORDS                                                         
*                                                                               
KWX      MVC   PAGE(24),=C'NAME         DESCRIPTION'                            
         USING CTAREC,R7                                                        
         BAS   RE,HIGH                                                          
         B     KWX4                                                             
         SPACE 1                                                                
KWX2     MVI   KEY+24,X'FF'                                                     
         BAS   RE,HIGH                                                          
         SPACE 1                                                                
KWX4     BAS   RE,FILTER                                                        
         BNE   KWX2                                                             
         CLC   USERID,CTAKUSER                                                  
         BNE   KWX2                                                             
         MVC   P,SPACES                                                         
         CLI   CTAKSUB,0                                                        
         BNE   KWX2                                                             
         MVC   P(6),CTAKID                                                      
         LR    R4,R7                                                            
         MVI   ELCODE,X'02'     DESCRIPTION ELEMENT                             
         BAS   RE,GETEL                                                         
         BNE   KWX6                                                             
         USING CTDSCD,R4                                                        
         SR    R3,R3                                                            
         IC    R3,CTDSCLEN                                                      
         SH    R3,=H'3'                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   P+13(0),CTDSC                                                    
KWX6     BAS   RE,ADD                                                           
         B     KWX2                                                             
         EJECT                                                                  
* COMMENT RECORDS                                                               
*                                                                               
COMMENT  MVC   PAGE(11),=C'COMMENT KEY'                                         
         MVC   PAGE+37(11),=C'DESCRIPTION'                                      
         MVC   PAGE+80(11),=C'-----------'                                      
         MVC   PAGE+117(11),=C'-----------'                                     
         LA    R7,KEY                                                           
         USING CTCREC,R7                                                        
         CLI   FILTAGY,0                                                        
         BE    COMM1                                                            
         OC    CTCKUSER,CTCKUSER                                                
         BNZ   COMM1                                                            
         MVI   CTCKUSER+1,1                                                     
COMM1    BAS   RE,HIGH                                                          
         B     COMM4                                                            
         SPACE 1                                                                
COMM2    MVI   KEY+24,X'FF'                                                     
COMM3    BAS   RE,HIGH                                                          
         SPACE 1                                                                
COMM4    LA    R7,IO                                                            
         BAS   RE,FILTER                                                        
         BNE   COMM2                                                            
         MVC   P,SPACES                                                         
         MVC   P(3),CTCKID                                                      
         LA    R3,P+2                                                           
         CLI   0(R3),C' '                                                       
         BNE   *+6                                                              
         BCTR  R3,0                                                             
         MVC   1(7,R3),=C',ID=ALL'                                              
         OC    CTCKUSER,CTCKUSER                                                
         BZ    COMM6                                                            
         XC    IO2(L'KEY),IO2                                                   
         MVI   IO2,C'I'                                                         
         MVC   IO2+23(2),CTCKUSER                                               
         GOTO1 DATAMGR,DMCB,(0,DMREAD),=C'CTFILE',IO2,IO2                       
         CLI   DMCB+8,0                                                         
         BNE   COMM2                                                            
         CLI   FILTAGY,0                                                        
         BE    COMM5                                                            
         LA    R4,IO2                                                           
         MVI   ELCODE,X'06'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         ZIC   R1,FILTAGY                                                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   2(0,R4),FILTAGY+1                                                
         BE    COMM5                                                            
         LA    R7,KEY                                                           
         ICM   R1,3,CTCKUSER                                                    
         XC    CTCKSYS(CTCLEN-CTCKSYS),CTCKSYS                                  
         LA    R1,1(R1)                                                         
         STCM  R1,3,CTCKUSER                                                    
         B     COMM3                                                            
COMM5    LA    R4,IO2                                                           
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BNE   COMM2                                                            
         MVC   5(10,R3),2(R4)                                                   
         SPACE 1                                                                
COMM6    LA    R3,14(R3)                                                        
         CLI   0(R3),C' '                                                       
         BNE   *+8                                                              
         BCT   R3,*-8                                                           
         MVC   1(8,R3),=C',SYS=ALL'                                             
         CLI   CTCKSYS,C' '                                                     
         BNH   COMM10                                                           
         L     R1,ASYSLST                                                       
         USING SYSLSTD,R1                                                       
COMM8    CLI   SYSLNUM,0                                                        
         BE    COMM2                                                            
         CLC   SYSLNAME(1),CTCKSYS                                              
         BE    *+12                                                             
         LA    R1,SYSLLEN(R1)                                                   
         B     COMM8                                                            
         MVC   6(L'SYSLNAME,R3),SYSLNAME                                        
         DROP  R1                                                               
         SPACE 1                                                                
COMM10   LA    R3,12(R3)                                                        
         CLI   0(R3),C' '                                                       
         BNE   *+8                                                              
         BCT   R3,*-8                                                           
         MVC   1(8,R3),=C',CLI=ALL'                                             
         CLI   CTCKID+3,C' '                                                    
         BNH   *+10                                                             
         MVC   6(5,R3),CTCKID+3                                                 
         OC    CTCKUSER,CTCKUSER                                                
         BZ    COMM12                                                           
         LA    R4,IO2                                                           
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BNE   COMM12                                                           
         ZIC   R1,1(R4)                                                         
         SH    R1,=H'3'                                                         
         CH    R1,=H'40'                                                        
         BNH   *+8                                                              
         LH    R1,=H'40'                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P+37(0),2(R4)                                                    
         SPACE 1                                                                
COMM12   BAS   RE,ADD                                                           
         B     COMM2                                                            
         EJECT                                                                  
* OUTPUT RECORDS                                                                
*                                                                               
OUT      MVC   PAGE(39),=C'OUTPUT  OUTPUT  OUTPUT   FORM  CARRIAGE'             
         MVC   PAGE+41(21),=C'NUMBER    DISP.  SEP='                            
         MVC   PAGE+80(38),=C' TYPE   CLASS  PRIORITY  CODE  CONTROL'           
         MVC   PAGE+121(07),=C'OF SETS'                                         
         MVC   PAGE+64(6),=C' TAPE '                                            
         MVC   PAGE+144(6),=C'DETAIL'                                           
         USING CTOREC,R7                                                        
         BAS   RE,HIGH                                                          
         B     OUT4                                                             
         SPACE 1                                                                
OUT2     BAS   RE,SEQ                                                           
         SPACE 1                                                                
OUT4     BAS   RE,FILTER                                                        
         BNE   OUT2                                                             
         MVC   P,SPACES                                                         
         MVC   P(6),CTOKID                                                      
         LR    R4,R7                                                            
         MVI   ELCODE,X'38'                                                     
         BAS   RE,GETEL                                                         
         BNE   OUT2                                                             
         USING CTOUTD,R4                                                        
         MVC   P+10(1),CTOUTCLS                                                 
         MVC   P+18(2),CTOUTPRI                                                 
         MVC   P+25(4),CTOUTPOW                                                 
         MVC   P+33(4),CTOUTCC                                                  
         MVC   P+43(1),CTOUTCPY                                                 
         MVC   P+53(1),CTOUTDIS                                                 
         MVC   P+58(1),CTOUTSEP                                                 
         MVC   P+64(6),CTOUTTAP                                                 
         BAS   RE,ADD                                                           
         B     OUT2                                                             
         EJECT                                                                  
* PROFILE RECORDS                                                               
*                                                                               
PROF     MVC   PAGE(34),=C'SYSTEM/ ID     P P P C OUTPUT ATTN'                  
         MVC   PAGE+35(24),=C'TEST DEST.  PROGRAM NAME'                         
         MVC   PAGE+80(34),=C'PROGRAM        P T O L  TYPE  TYPE'               
         MVC   PAGE+115(8),=C'PHASE ID'                                         
         USING CTPREC,R7                                                        
         BAS   RE,HIGH                                                          
         B     PROF4                                                            
         SPACE 1                                                                
PROF2    BAS   RE,SEQ                                                           
         SPACE 1                                                                
PROF4    BAS   RE,FILTER                                                        
         BNE   PROF2                                                            
         MVC   P,SPACES                                                         
         MVC   P+1(1),CTPKSYS                                                   
         MVI   P+2,C'/'                                                         
         MVC   P+3(2),CTPKPROG                                                  
         OC    CTPKORIG,CTPKORIG   FOR OVERRIDES, NEED TO READ ID'S             
         BZ    PROF6                                                            
         MVC   IO2(L'KEY),KEY                                                   
         XC    KEY,KEY                                                          
         MVI   KEY,C'I'                                                         
         MVC   KEY+23(2),IO2+22                                                 
         EDIT  (2,KEY+23),(5,P+8),ALIGN=LEFT                                    
         BAS   RE,HIGH                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BNE   PROF5                                                            
         LR    R4,R7                                                            
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BNE   PROF5                                                            
         USING CTDSCD,R4                                                        
         SR    R3,R3                                                            
         IC    R3,CTDSCLEN                                                      
         SH    R3,=H'3'                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   P+8(0),CTDSC                                                     
         SPACE 1                                                                
PROF5    MVC   KEY,IO2                                                          
         BAS   RE,READ                                                          
         SPACE 1                                                                
PROF6    MVI   ELCODE,0                                                         
         LR    R4,R7                                                            
         BAS   RE,GETEL                                                         
         B     PROF10                                                           
         SPACE 1                                                                
PROF8    BAS   RE,NEXTEL                                                        
         SPACE 1                                                                
PROF10   IF    0(R4),=,0,PROF12                                                 
         IF    0(R4),=,X'02',PROF14                                             
         CLI   FILTPROF,C' '                                                    
         BE    PROF11                                                           
         CLC   2(1,R4),FILTPROF                                                 
         BNE   PROF8                                                            
         SPACE 1                                                                
PROF11   CLI   2(R4),C'T'                                                       
         BNE   *+8                                                              
         MVI   P+6,C'T'                                                         
         IF    0(R4),=,X'40',PROF16                                             
         IF    0(R4),=,X'41',PROF18                                             
         IF    0(R4),=,X'42',PROF20                                             
         IF    0(R4),=,X'44',PROF22                                             
         IF    0(R4),=,X'45',PROF24                                             
         IF    0(R4),=,X'4E',PROF26                                             
         B     PROF8                                                            
         SPACE 1                                                                
PROF12   BAS   RE,ADD                                                           
         B     PROF2                                                            
         SPACE 1                                                                
         USING CTDSCD,R4                                                        
PROF14   SR    R3,R3                                                            
         IC    R3,CTDSCLEN                                                      
         SH    R3,=H'3'                                                         
         MVC   WORK,SPACES                                                      
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),CTDSC                                                    
         GOTO1 CHOPPER,DMCB,(80,WORK),(32,P+47),1                               
         B     PROF8                                                            
         SPACE 1                                                                
         USING CTDCOD,R4                                                        
PROF16   MVC   P+40(6),CTDCODE                                                  
         B     PROF8                                                            
         SPACE 1                                                                
         USING CTACOD,R4                                                        
PROF18   MVC   P+30(3),CTACODE                                                  
         B     PROF8                                                            
         SPACE 1                                                                
         USING CTOCOD,R4                                                        
PROF20   MVC   P+23(6),CTOCODE                                                  
         B     PROF8                                                            
         SPACE 1                                                                
         USING CTPRID,R4                                                        
PROF22   MVC   P+15(1),CTPRITY                                                  
         CLI   CTPRILEN,8                                                       
         BE    PROF8                                                            
         MVC   P+17(1),CTPRIPT                                                  
         MVC   P+19(1),CTPRIPO                                                  
         B     PROF8                                                            
         SPACE 1                                                                
         USING CTRCLD,R4                                                        
PROF24   MVC   P+21(1),CTRCLASS                                                 
         B     PROF8                                                            
         SPACE 1                                                                
         USING CTPHSD,R4                                                        
PROF26   MVC   P+35(4),CTPHS01                                                  
         B     PROF8                                                            
         EJECT                                                                  
* TERMINALS                                                                     
*                                                                               
TERM     MVC   PAGE(39),=C'LINE ADDR PASSWORD   SYSTEMS AUTHORIZED'             
         MVC   PAGE+52(7),=C'ID LIST'                                           
         USING CTTREC,R7                                                        
         BAS   RE,HIGH                                                          
         B     TERM4                                                            
         SPACE 1                                                                
TERM2    BAS   RE,SEQ                                                           
         SPACE 1                                                                
TERM4    BAS   RE,FILTER                                                        
         BNE   TERM2                                                            
         MVC   P,SPACES                                                         
         MVC   P(4),CTTKLINE                                                    
         MVC   P+5(4),CTTKADDR                                                  
         MVC   P+10(10),CTTKPASS                                                
         OC    P+10(10),SPACES                                                  
         BAS   RE,AUTH                                                          
         GOTO1 CHOPPER,DMCB,(64,WORK),(30,P+21),1                               
         BAS   RE,IDLIST                                                        
         BAS   RE,ADD                                                           
         B     TERM2                                                            
         EJECT                                                                  
* SYSTEMS AUTHORIZED                                                            
*                                                                               
AUTH     NTR1                                                                   
         MVC   WORK,SPACES                                                      
         LA    R3,WORK                                                          
         LR    R4,R7                                                            
         MVI   ELCODE,X'21'                                                     
         USING CTSYSD,R4                                                        
         BAS   RE,GETEL                                                         
         B     AUTH4                                                            
         SPACE 1                                                                
AUTH2    BAS   RE,NEXTEL                                                        
         SPACE 1                                                                
AUTH4    BNE   AUTH10                                                           
         L     R1,ASYSLST                                                       
         USING SYSLSTD,R1                                                       
         SPACE 1                                                                
AUTH6    CLI   SYSLNUM,0                                                        
         BE    AUTH2                                                            
         CLC   SYSLNUM,CTSYSNUM                                                 
         BE    AUTH8                                                            
         LA    R1,SYSLLEN(R1)                                                   
         B     AUTH6                                                            
         SPACE 1                                                                
AUTH8    MVC   0(7,R3),SYSLNAME                                                 
         LA    R3,8(R3)                                                         
         B     AUTH2                                                            
         DROP  R1                                                               
         SPACE 1                                                                
AUTH10   GOTO1 SQUASHER,DMCB,WORK,64                                            
         B     XIT                                                              
         EJECT                                                                  
* ID LIST                                                                       
*                                                                               
IDLIST   NTR1                                                                   
         MVC   WORK,SPACES                                                      
         LA    R3,WORK                                                          
         LA    R5,7                                                             
         LR    R4,R7                                                            
         MVI   ELCODE,X'1F'        PRINCIPAL ID                                 
         BAS   RE,GETEL                                                         
         BNE   IDLIST1                                                          
         USING CTPID,R4                                                         
         MVC   0(10,R3),CTPID                                                   
         LA    R3,11(R3)                                                        
         BCTR  R5,0                                                             
IDLIST1  LR    R4,R7                                                            
         MVI   ELCODE,X'20'                                                     
         USING CTIDD,R4                                                         
         BAS   RE,GETEL                                                         
         B     IDLIST4                                                          
         SPACE 1                                                                
IDLIST2  BAS   RE,NEXTEL                                                        
         SPACE 1                                                                
IDLIST4  BNE   IDLIST6                                                          
         MVC   0(10,R3),CTID                                                    
         LA    R3,11(R3)                                                        
         BCT   R5,IDLIST2                                                       
         SPACE 1                                                                
IDLIST6  GOTO1 SQUASHER,DMCB,WORK,80                                            
         GOTO1 CHOPPER,DMCB,(80,WORK),(27,P+52),1                               
         B     XIT                                                              
         EJECT                                                                  
* USER PROFILE RECORDS                                                          
*                                                                               
         USING CTUREC,R7                                                        
USER     MVC   PAGE(160),USERHEAD                                               
         BAS   RE,HIGH                                                          
         B     USER4                                                            
         SPACE 1                                                                
USER2    DS    0H                                                               
*&&US                                                                           
         OC    CTUKAGY,CTUKAGY     IF CURRENT RECORD IS A FIELD RECORD          
         BNZ   USER3                                                            
         MVC   KEY,0(R7)                                                        
         LA    R7,KEY                                                           
         MVI   CTUKAGY,1           FORCE A READ OF USER FILE TO GET             
         LA    R7,IO                                                            
         BAS   RE,HIGH             USER PROFILES                                
         B     USER4                                                            
*&&                                                                             
         SPACE 1                                                                
*                                  READ NEXT RECORD HIGH WITH KEY               
USER3    MVI   KEY+24,X'FF'          INCREMENT AS SEQUENCE MIGHT BE             
         BAS   RE,HIGH               BROKEN BY READ FOR USER ID RECORD          
         SPACE 1                                                                
USER4    BAS   RE,FILTER                                                        
         BNE   USER2                                                            
         MVC   P,SPACES                                                         
         LA    R3,P                                                             
         MVC   0(1,R3),CTUKSYS     DISPLAY KEY (SPP/AG/M/CLI)                   
         OI    0(R3),X'40'                                                      
         MVC   1(2,R3),CTUKPROG+1                                               
         CLI   CTUKPROG,0                                                       
         BNE   *+12                                                             
         LA    R3,3(R3)                                                         
         B     USER5                                                            
         MVC   1(3,R3),CTUKPROG                                                 
         LA    R3,4(R3)                                                         
USER5    OC    CTUKAGY,CTUKAGY     FIELD RECORD                                 
         BZ    USER10                                                           
         MVI   0(R3),C'/'                                                       
         CLI   CTUKAGY,X'40'       CHECK IF USER ID LEVEL PROFILE               
         BL    USER5A                                                           
         MVC   1(2,R3),CTUKAGY                                                  
         CLI   CTUKSYS,C'A'        IF IT'S ACCOUNT                              
         BE    USER6A              SPECIAL ROUTINE                              
         LA    R3,3(R3)                                                         
         B     USER6                                                            
*                                  HERE IF USER ID LEVEL PROFILE                
USER5A   XC    IO2(L'KEY),IO2      GET USER ID                                  
         MVI   IO2,C'I'                                                         
         MVC   IO2+23(2),CTUKAGY                                                
         GOTO1 DATAMGR,DMCB,(0,DMREAD),=C'CTFILE',IO2,IO2                       
         CLI   DMCB+8,0                                                         
         BNE   USER2                                                            
         LA    R4,IO2                                                           
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BNE   USER2                                                            
         MVC   1(10,R3),2(R4)      MOVE USER ID TO DISPLAY LINE                 
         LA    R3,10(R3)                                                        
         CLI   0(R3),C' '          POINT TO FIRST SPACE AFTER ID                
         BNE   *+8                                                              
         BCT   R3,*-8                                                           
         LA    R3,1(R3)                                                         
         CLI   CTUKSYS,C'A'        IF IT'S ACCOUNT                              
         BE    USER6A5             SPECIAL ROUTINE                              
*                                                                               
USER6    OC    CTUKCLT,CTUKCLT     HERE FOR MEDIA SYSTEM PROFILE                
         BNZ   *+12                                                             
         CLI   CTUKMED,0                                                        
         BE    USER8                                                            
         MVI   0(R3),C'/'                                                       
         LA    R3,1(R3)                                                         
*                                  OTHER SYSTEMS                                
         MVC   0(1,R3),CTUKMED                                                  
         CLI   CTUKMED,0                                                        
         BNE   *+8                                                              
         MVI   0(R3),C'*'          *=ALLMEDIA                                   
         OC    CTUKCLT,CTUKCLT                                                  
         BZ    USER8                                                            
         MVI   1(R3),C'/'                                                       
         MVC   2(3,R3),CTUKCLT                                                  
         TM    CTUKCLT+1,X'C0'     DEAL WITH UK 5 CHR CLIENT CODES              
         BNZ   USER8                                                            
         MVC   DUB(2),CTUKCLT+1                                                 
         LH    R1,DUB                                                           
         CVD   R1,DUB                                                           
         UNPK  3(4,R3),DUB                                                      
         OI    6(R3),X'F0'                                                      
         B     USER8                                                            
*                                  SPECIAL CODE FOR ACCOUNT SYSTEM              
USER6A   L     R1,ASYSLST                                                       
         USING SYSLSTD,R1                                                       
USER6A1  CLI   SYSLNUM,6           FIND ENTRY FOR ACC                           
         BE    *+12                                                             
         LA    R1,SYSLLEN(R1)                                                   
         B     USER6A1                                                          
         TM    SYSLIND1,X'80'                                                   
         BO    USER6A2                                                          
         LA    R3,3(R3)                                                         
         B     USER6A5             BRANCH IF ACC HAS BEEN CONVERTED             
USER6A2  GOTO1 HEXOUT,DMCB,CTUKAGY,1(R3),1,=C'TOG'                              
         LA    R3,3(R3)                                                         
         OC    CTUKCLT,CTUKCLT                                                  
         BNZ   *+12                                                             
         CLI   CTUKMED,0                                                        
         BE    USER8                                                            
         MVC   0(2,R3),=C'/*'                                                   
         OC    CTUKAGY+1(2),CTUKAGY+1                                           
         BZ    USER6A3                                                          
         LA    R3,1(R3)                                                         
         MVC   0(2,R3),CTUKAGY+1                                                
*&&US                                                                           
         CLI   CTUKAGY+1,C'T'      IF UNIT IS T (TALENT COMM'LS)                
         BNE   USER6A3                                                          
         GOTO1 HEXOUT,DMCB,CTUKAGY+2,1(R3),1,=C'TOG' DISP LEDG IN HEX           
         LA    R3,1(R3)                                                         
*&&                                                                             
USER6A3  LA    R3,2(R3)                                                         
         OC    CTUKCLT,CTUKCLT                                                  
         BZ    USER8                                                            
         MVI   0(R3),C'/'                                                       
         MVC   1(3,R3),CTUKCLT                                                  
         B     USER8                                                            
*                                  IF ACC HAS BEEN CONVERTED                    
USER6A5  OC    CTUKUNT,CTUKUNT     UNIT AND LEDGER                              
         BZ    USER6A8                                                          
         MVI   0(R3),C'/'                                                       
         LA    R3,1(R3)                                                         
         MVC   0(2,R3),CTUKUNT                                                  
*&&US                                                                           
         CLI   CTUKUNT,C'T'        IF UNIT IS T (TALENT COMM'LS)                
         BNE   USER6A6                                                          
         GOTO1 HEXOUT,DMCB,CTUKLDG,1(R3),1,=C'TOG' DISPLAY LEDG IN HEX          
         LA    R3,1(R3)                                                         
*&&                                                                             
USER6A6  LA    R3,2(R3)                                                         
USER6A8  OC    CTUKACT,CTUKACT     ACCOUNT                                      
         BZ    USER8                                                            
         MVI   0(R3),C'/'                                                       
         MVC   1(3,R3),CTUKACT                                                  
         B     USER8                                                            
*                                                                               
         SPACE 1                                                                
USER8    CLI   CTUKAGY,X'40'       CHECK IF USER ID LEVEL PROFILE               
         BNL   USER8A                                                           
         BAS   RE,ADD              IF SO NEED TO PRINT DATA ON 2 LINES          
         MVC   P,SPACES                                                         
USER8A   LR    R4,R7                                                            
         MVI   ELCODE,X'72'                                                     
         BAS   RE,GETEL                                                         
         BNE   USER2                                                            
         USING CTPVD,R4                                                         
         MVC   BLOCK(16),CTPVALUE                                               
         B     USER18                                                           
         SPACE 1                                                                
USER10   LA    R4,CTUDATA          NEW SYSTEM/PROGRAM EXTRACT VALUES            
         XC    LASTUSER,LASTUSER                                                
         XC    BLOCK(16),BLOCK                                                  
USER12   CLI   0(R4),0                                                          
         BE    USER18                                                           
         CLI   0(R4),X'70'                                                      
         BE    USER16                                                           
USER14   ZIC   R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     USER12                                                           
         USING CTFDD,R4                                                         
USER16   ZIC   R1,CTFDNUM                                                       
         LR    RE,R1                                                            
         LA    R1,BLOCK-1(R1)                                                   
         LA    RE,LASTUSER-1(RE)                                                
         OI    0(RE),X'80'         SET FIELD PRESENT                            
         TM    CTFDOTHR,X'80'                                                   
         BZ    *+8                                                              
         OI    0(RE),X'40'         SET DDS INDIC                                
         TM    CTFDOTHR,X'40'                                                   
         BZ    *+8                                                              
         OI    0(RE),X'20'         SET DEFAULT INDIC                            
         CLI   CTFDTYPE,C'N'                                                    
         BNE   *+8                                                              
         OI    0(RE),X'08'         NUMERIC FIELD                                
         CLI   CTFDTYPE,C'C'                                                    
         BNE   *+8                                                              
         OI    0(RE),X'04'         ALPHA FIELD                                  
         CLI   CTFDTYPE,C'X'                                                    
         BNE   *+8                                                              
         OI    0(RE),X'02'         HEX FIELD                                    
         MVC   0(1,R1),CTFDDEF                                                  
         B     USER14                                                           
         SPACE 1                                                                
USER18   LA    R3,P+15             DISPLAY PROFILE VALUES                       
         LA    R4,BLOCK                                                         
         LA    R5,LASTUSER                                                      
         LA    R6,16                                                            
         SPACE 1                                                                
USER20   TM    0(R5),X'80'                                                      
         BZ    USER26                                                           
         CLI   0(R4),0                                                          
         BNE   *+12                                                             
         TM    0(R5),X'20'                                                      
         BO    USER24                                                           
         MVC   2(1,R3),0(R4)                                                    
         TM    0(R5),X'04'                                                      
         BO    USER24                                                           
         TM    0(R5),X'08'                                                      
         BZ    USER22                                                           
         EDIT  (B1,0(R4)),(3,0(R3))                                             
         OI    2(R3),X'F0'                                                      
         B     USER24                                                           
USER22   GOTO1 HEXOUT,DMCB,0(R4),1(R3),1,=C'TOG'                                
USER24   TM    0(R5),X'40'                                                      
         BZ    USER26                                                           
         MVI   3(R3),C'*'                                                       
         SPACE 1                                                                
USER26   LA    R3,4(R3)                                                         
         LA    R4,1(R4)                                                         
         LA    R5,1(R5)                                                         
         BCT   R6,USER20                                                        
         SPACE 1                                                                
         BAS   RE,ADD                                                           
         B     USER2                                                            
         SPACE 1                                                                
USERHEAD DC    CL40'SPPP/AG/M/CLI   --------DEFAULTS AND EFF'                   
         DC    CL40'ECTIVE VALUES FOR FIELD NUMBERS-------  '                   
         DC    CL40'------------    01  02  03  04  05  06  '                   
         DC    CL40'07  08  09  10  11  12  13  14  15  16  '                   
         EJECT                                                                  
* CPP EXTRACT RECORDS                                                           
*                                                                               
         USING CTXREC,R7                                                        
XTRACT   MVC   PAGE(43),=C'AGY CLT PRD EST START    END      DEMO LIST'         
         MVC   PAGE+50(20),=C'PROGTYPE EQUIVALENCE'                             
         BAS   RE,HIGH                                                          
         B     XT4                                                              
         SPACE 1                                                                
XT2      BAS   RE,SEQ                                                           
         SPACE 1                                                                
XT4      BAS   RE,FILTER                                                        
         BNE   XT2                                                              
         MVC   P,SPACES                                                         
         MVC   P(2),CTXKAGY                                                     
         MVC   P+4(3),CTXKCLT                                                   
         MVC   P+8(3),CTXKPRD                                                   
         EDIT  (1,CTXKEST),(3,P+12),ZERO=BLANK                                  
         OC    P(16),SPACES                                                     
         OC    CTXKSTRT,CTXKSTRT                                                
         BZ    XT6                                                              
         GOTO1 DATCON,DMCB,(1,CTXKSTRT),(8,P+16)                                
         SPACE 1                                                                
XT6      OC    CTXKEND,CTXKEND                                                  
         BZ    XT8                                                              
         GOTO1 DATCON,DMCB,(1,CTXKEND),(8,P+25)                                 
         SPACE 1                                                                
XT8      MVI   ELCODE,X'74'                                                     
         LR    R4,R7                                                            
         BAS   RE,GETEL                                                         
         BNE   XT16                                                             
         USING CTDXD,R4                                                         
         ZIC   R3,CTDXLEN                                                       
         SH    R3,=H'2'                                                         
         LA    R5,CTDXLIST                                                      
         LA    R6,P+34                                                          
         SPACE 1                                                                
XT10     TM    0(R5),X'F0'                                                      
         BO    XT12                                                             
         EDIT  (1,0(R5)),(2,0(R6))                                              
         B     XT14                                                             
         SPACE 1                                                                
XT12     EDIT  (1,0(R5)),(1,1(R6))                                              
         MVI   0(R6),C'F'                                                       
         SPACE 1                                                                
XT14     LA    R5,1(R5)                                                         
         LA    R6,3(R6)                                                         
         BCT   R3,XT10                                                          
         SPACE 1                                                                
XT16     LR    R4,R7                                                            
         MVI   ELCODE,X'76'                                                     
         LA    R5,5                                                             
         BAS   RE,GETEL                                                         
         LA    R6,P+50                                                          
         B     XT20                                                             
         SPACE 1                                                                
XT18     BAS   RE,NEXTEL                                                        
         SPACE 1                                                                
         USING CTPQD,R4                                                         
XT20     BNE   XT22                                                             
         MVC   0(3,R6),CTPQDP                                                   
         OI    0(R6),X'40'                                                      
         OI    0(R6),X'40'                                                      
         MVI   3(R6),C'='                                                       
         MVC   4(1,R6),CTPQCODE                                                 
         LA    R6,6(R6)                                                         
         BCT   R5,XT18                                                          
         BAS   RE,ADD                                                           
         LA    R5,5                                                             
         LA    R6,P+50                                                          
         B     XT18                                                             
         SPACE 1                                                                
XT22     BAS   RE,ADD                                                           
         B     XT2                                                              
         EJECT                                                                  
* CPP PROJECTION FORMULAS                                                       
*                                                                               
         USING CTYREC,R7                                                        
FORM     MVC   PAGE+00(39),=C'AGENCY   FORMULA   FORMULA   PROJECTION'          
         MVC   PAGE+80(39),=C' CODE     CODE      MONTH      FACTOR  '          
         BAS   RE,HIGH                                                          
         B     FORM4                                                            
         SPACE 1                                                                
FORM2    BAS   RE,SEQ                                                           
         SPACE 1                                                                
FORM4    BAS   RE,FILTER                                                        
         BNE   FORM2                                                            
         MVC   P,SPACES                                                         
         MVC   P+2(2),CTYKAGY                                                   
         MVC   P+12(1),CTYKFORM                                                 
         GOTO1 DATCON,DMCB,(3,CTYKYEAR),(6,P+20)                                
         LR    R4,R7                                                            
         MVI   ELCODE,X'78'                                                     
         BAS   RE,GETEL                                                         
         BNE   FORM6                                                            
         USING CTPFD,R4                                                         
         EDIT  (2,CTPFACT),(5,P+32)                                             
         SPACE 1                                                                
FORM6    BAS   RE,ADD                                                           
         B     FORM2                                                            
         EJECT                                                                  
* SECRET AUTHORIZATION CODES (BY NAME)                                          
*                                                                               
         USING CT0REC,R7                                                        
NAME     MVC   PAGE(39),=C'TYPE -------- NAME -------- SECRET CODE'             
         MVC   PAGE+40(17),=C'LAST CHG  SYSTEMS'                                
*                                                                               
         BAS   RE,HIGH                                                          
         B     NAME4                                                            
*                                                                               
NAME2    BAS   RE,SEQ                                                           
*                                                                               
NAME4    BAS   RE,FILTER           TEST E-O-F                                   
         CLC   KEY(3),KEYSAVE      TEST SAME AGY                                
         BE    NAME6                                                            
         MVI   KEY,0               FORCE KEY NEQ KEYSAVE                        
         B     FILTER               AND EXIT FROM FILTER ROUTINE                
*                                                                               
NAME6    TM    CT0STAT,X'20'       IS THIS A LOCKED RECORD???                   
         BNZ   NAME2               YES? SUPRESS OUTPUT                          
         MVC   P,SPACES                                                         
         MVC   P+1(2),CT0KOFFC     TYPE                                         
         MVC   P+6(18),CT0KLAST    LAST                                         
         MVC   P+26(1),CT0KFI      F                                            
         MVC   P+28(1),CT0KMI      M                                            
         GOTO1 SQUASHER,DMCB,P+6,23                                             
*                                                                               
         MVI   ELCODE,03                                                        
         LR    R4,R7                                                            
         BAS   RE,GETEL                                                         
         B     *+8                                                              
NAME8    BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   1(R4),12                                                         
         BNE   NAME8                                                            
         MVC   P+29(10),2(R4)                                                   
*                                                                               
         MVI   ELCODE,01                                                        
         LR    R4,R7                                                            
         BAS   RE,GETEL                                                         
         BNE   NAME9               **NOP** BE *+6                               
**NOP**  DC    H'0'                                                             
         GOTO1 DATCON,DMCB,(3,2(R4)),(8,P+40)                                   
*                                                                               
NAME9    BAS   RE,AUTH                                                          
         GOTO1 CHOPPER,DMCB,(64,WORK),(30,P+50),1                               
         BAS   RE,ADD                                                           
         B     NAME2                                                            
         EJECT                                                                  
* SECRET AUTHORIZATION CODES (BY CODE)                                          
*                                                                               
         USING CT0REC,R7                                                        
CODE     MVC   PAGE(39),=C'SECRET CODE TYPE -------- NAME --------'             
         MVC   PAGE+40(17),=C'LAST CHG  SYSTEMS'                                
*                                                                               
         BAS   RE,HIGH                                                          
         B     CODE4                                                            
*                                                                               
CODE2    BAS   RE,SEQ                                                           
*                                                                               
CODE4    BAS   RE,FILTER           TEST E-O-F                                   
         CLC   KEY(3),KEYSAVE      TEST SAME AGY                                
         BE    *+12                                                             
         MVI   KEY,0               FORCE KEY NEQ KEYSAVE                        
         B     FILTER               AND EXIT FROM FILTER ROUTINE                
         OC    KEY+3(3),KEY+3      TEST NAME PRESENT                            
         BZ    CODE6               NO - CONTINUE                                
         MVI   KEY,0               FORCE KEY NEQ KEYSAVE                        
         B     FILTER               AND EXIT WITH MESSAGE                       
*                                                                               
CODE6    TM    CT0STAT,X'20'       IS THIS A LOCKED RECORD?                     
         BNZ   CODE2               SUPRESS OUTPUT                               
         MVC   P,SPACES                                                         
         MVC   P(10),CT0KCODE                                                   
*                                                                               
         MVI   ELCODE,X'03'        SEARCH FOR AUTH NAME ELEMENT                 
         LR    R4,R7                                                            
         BAS   RE,GETEL                                                         
         B     *+8                                                              
CODE8    BAS   RE,NEXTEL                                                        
         BNE   CODE8A                                                           
         CLI   1(R4),24                                                         
         BNE   CODE8                                                            
         MVC   P+13(2),2(R4)                                                    
         MVC   P+18(18),4(R4)                                                   
         MVC   P+38(1),22(R4)                                                   
         MVC   P+40(1),23(R4)                                                   
         GOTO1 SQUASHER,DMCB,P+18,23                                            
         B     CODE8B                                                           
*                                                                               
CODE8A   MVI   ELCODE,X'C3'        SEARCH FOR PERSONAL ID ELEMENT               
         LR    R4,R7                                                            
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   P+13(8),2(R4)                                                    
*                                                                               
CODE8B   LR    R4,R7                                                            
         MVI   ELCODE,1                                                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATCON,DMCB,(3,2(R4)),(8,P+40)                                   
*                                                                               
         BAS   RE,AUTH                                                          
         GOTO1 CHOPPER,DMCB,(64,WORK),(30,P+50),1                               
         BAS   RE,ADD                                                           
         B     CODE2                                                            
         EJECT                                                                  
* FILTER                                                                        
*                                                                               
FILTER   ST    RE,MYRE                                                          
         CLC   KEYSAVE(1),KEY                                                   
         BE    FILTER2                                                          
         MVC   INFMES,SPACES                                                    
         MVC   INFMES(28),=C'END OF RECORDS FOR THIS TYPE'                      
         BAS   RE,PAGEOUT                                                       
         LA    R2,INFRECH                                                       
         B     OKEXIT                                                           
         SPACE 1                                                                
FILTER2  OC    FILTID,FILTID                                                    
         BZ    FILTER8                                                          
         CLI   KEY,C'P'                                                         
         BE    FILTER3                                                          
         CLI   KEY,C'A'                                                         
         BE    FILTERK                                                          
         LR    R4,R7                                                            
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         B     FILTER6                                                          
         SPACE 1                                                                
FILTERK  LR    R4,R7                                                            
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BNE   FILTERNO                                                         
         B     FILTERK2                                                         
FILTERK1 BAS   RE,NEXTEL                                                        
         BNE   FILTERNO                                                         
         USING CTPASD,R4                                                        
FILTERK2 CLC   FILTIDNO,CTPASDTA                                                
         BNE   FILTERK1                                                         
         B     FILTERYS                                                         
         SPACE 1                                                                
FILTER3  LR    R4,R7                                                            
         USING CTPREC,R4                                                        
         CLC   CTPKORIG,FILTIDNO                                                
         BNE   FILTERNO                                                         
         B     FILTER8                                                          
         SPACE 1                                                                
FILTER4  BAS   RE,NEXTEL                                                        
         SPACE 1                                                                
FILTER6  BNE   FILTERNO                                                         
         USING CTIDD,R4                                                         
         CLC   CTID,FILTID                                                      
         BNE   FILTER4                                                          
         SPACE 1                                                                
FILTER8  OC    FILTPASS,FILTPASS   PASSWORD FILTER FOR TERMINALS                
         BZ    FILTER10                                                         
         USING CTTREC,R7                                                        
         OC    CTTKPASS,CTTKPASS                                                
         BZ    FILTERNO                                                         
         CLC   FILTPASS(4),=C'ALL '                                             
         BE    FILTERYS                                                         
         CLC   CTTKPASS,FILTPASS                                                
         BE    FILTERYS                                                         
         B     FILTERNO                                                         
         SPACE 1                                                                
FILTER10 OC    FILTOUT,FILTOUT                                                  
         BZ    FILTER12                                                         
         LR    R4,R7                                                            
         MVI   ELCODE,X'42'                                                     
         BAS   RE,GETEL                                                         
         BNE   FILTERNO                                                         
         USING CTOCOD,R4                                                        
         CLC   FILTOUT,CTOCODE                                                  
         BNE   FILTERNO                                                         
         SPACE 1                                                                
FILTER12 CLI   FILTAGY,0                                                        
         BE    FILTER14                                                         
         USING CTUREC,R7                                                        
         CLI   CTUKTYP,C'I'        IF TYPE IS ID                                
         BE    FILTER13            THIS IS A FILTER ON 2-CHAR ALPHA             
*&&UK*&& CLI   CTUKTYP,C'U'        UK USER PROFILES ONLY                        
*&&UK*&& BNE   FILTERYS                                                         
         OC    CTUKAGY,CTUKAGY                                                  
         BZ    FILTERYS                                                         
         ZIC   R1,FILTAGY                                                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   CTUKAGY(0),FILTAGY+1                                             
         BE    FILTERYS                                                         
         B     FILTERNO                                                         
         SPACE 1                                                                
FILTER13 MVI   ELCODE,X'06'        2-CHAR ALPHA FILTER FOR ID RECS              
         LR    R4,R7                                                            
         BAS   RE,GETEL                                                         
         BNE   FILTERNO                                                         
         USING CTAGYD,R4                                                        
         CLC   CTAGYID,FILTAGY+1                                                
         BE    FILTERYS                                                         
         B     FILTERNO                                                         
         DROP  R4                                                               
         SPACE 1                                                                
FILTER14 DS    0H                                                               
         SPACE 1                                                                
FILTERYS SR    R1,R1                                                            
         B     *+8                                                              
FILTERNO LA    R1,1                                                             
         LTR   R1,R1                                                            
         L     RE,MYRE                                                          
         BR    RE                                                               
         EJECT                                                                  
* ADD A LINE TO PAGE - CHECK FOR FULL                                           
*                                                                               
ADD      ST    RE,MYRE                                                          
         LA    R2,PAGE+160                                                      
         LA    R3,16                                                            
         SPACE 1                                                                
ADD2     CLC   0(80,R2),SPACES                                                  
         BE    ADD4                                                             
         LA    R2,80(R2)                                                        
         BCT   R3,ADD2                                                          
         BAS   RE,PAGEOUT          PAGE IS FULL                                 
         MVC   INFMES,SPACES                                                    
         MVC   INFMES(38),=C'RECORDS DISPLAYED - HIT ENTER FOR NEXT'            
         MVC   INFKEY,=CL9'NEXT'                                                
         OI    INFKEYH+6,X'80'                                                  
         MVC   LASTKEY,KEY                                                      
         LA    R2,INFTABH                                                       
         OI    6(R2),X'01'         MODIFY FIELD                                 
         B     OKEXIT                                                           
         SPACE 1                                                                
ADD4     MVC   0(80,R2),P                                                       
         MVC   P,SPACES                                                         
         L     RE,MYRE                                                          
         BR    RE                                                               
         EJECT                                                                  
* ROUTINE TO HANDLE PAGE                                                        
*                                                                               
PAGECLR  NTR1                                                                   
         LA    R4,PAGE                                                          
         LA    R5,18                                                            
         SPACE 1                                                                
PAGE2    MVC   0(80,R4),SPACES                                                  
         LA    R4,80(R4)                                                        
         BCT   R5,PAGE2                                                         
         B     XIT                                                              
         SPACE 1                                                                
PAGEOUT  NTR1                                                                   
         LA    R2,INFSUBAH                                                      
         LA    R3,18                                                            
         LA    R4,PAGE                                                          
         SPACE 1                                                                
PAGE4    CLC   8(78,R2),0(R4)                                                   
         BE    PAGE6                                                            
         MVC   8(78,R2),0(R4)                                                   
         OI    6(R2),X'80'                                                      
         SPACE 1                                                                
PAGE6    LA    R2,86(R2)                                                        
         LA    R4,80(R4)                                                        
         BCT   R3,PAGE4                                                         
         B     XIT                                                              
         EJECT                                                                  
* DATA MANAGER AND OTHER AIDS                                                   
*                                                                               
READ     LA    RF,DMREAD                                                        
         B     LINK                                                             
         SPACE 1                                                                
HIGH     LA    RF,DMRDHI                                                        
         MVC   KEYSAVE,KEY                                                      
         B     LINK                                                             
         SPACE 1                                                                
SEQ      LA    RF,DMRSEQ                                                        
         MVC   KEYSAVE,KEY                                                      
         B     LINK                                                             
         SPACE 1                                                                
LINK     NTR1                                                                   
         ST    RF,DMCB                                                          
         GOTO1 DATAMGR,DMCB,,=C'CTFILE',KEY,IO,0                                
         CLI   DMCB+8,0                                                         
         BNE   LINK2                                                            
         MVC   KEY,IO                                                           
         B     XIT                                                              
         SPACE 1                                                                
LINK2    L     RD,4(RD)            UNWIND                                       
         LM    RE,RC,12(RD)                                                     
         MVI   ERROR,0                                                          
         SPACE 1                                                                
ERREX    GOTO1 GETMSG,DMCB+12,(ERROR,INFMES),(10,DMCB),DATAMGR                  
         SPACE 1                                                                
OKEXIT   OI    6(R2),X'40'+X'80'   INSERT CURSOR AND TRANSMIT                   
         OI    INFMESH+6,X'80'                                                  
         SPACE 1                                                                
XIT      XIT1                                                                   
         SPACE 1                                                                
DATADISP DC    H'28'                                                            
         GETEL (R4),DATADISP,ELCODE                                             
         EJECT                                                                  
* LITERALS ETC.                                                                 
*                                                                               
         LTORG                                                                  
         SPACE 1                                                                
DMREAD   DC    CL8'DMREAD'                                                      
DMRDHI   DC    CL8'DMRDHI'                                                      
DMRSEQ   DC    CL8'DMRSEQ'                                                      
         SPACE 1                                                                
RECTAB   DS    0XL14                                                            
         DC    C'ERROR    E',X'00',AL3(ER)                                      
         DC    C'ID       I',X'00',AL3(ID)                                      
         DC    C'ID#2     I',X'00',AL3(DEST)                                    
         DC    C'ID#3     I',X'00',AL3(ATT)                                     
         DC    C'IDINFO   I',X'00',AL3(DEST)                                    
         DC    C'IDATTN   I',X'00',AL3(ATT)                                     
         DC    C'JCL      J',X'00',AL3(JCL)                                     
         DC    C'KWXLIST  A',X'00',AL3(KWX)                                     
         DC    C'COMMENT  C',X'00',AL3(COMMENT)                                 
         DC    C'LIBRARY  L',X'00',AL3(LIB)                                     
         DC    C'OUTPUT   O',X'00',AL3(OUT)                                     
         DC    C'PROFILE  P',X'00',AL3(PROF)                                    
         DC    C'TERMINAL T',X'00',AL3(TERM)                                    
         DC    C'DEST     I',X'00',AL3(DEST)                                    
         DC    C'ATTENTIONI',X'00',AL3(ATT)                                     
         DC    C'USER     U',X'00',AL3(USER)                                    
         DC    C'FIELD    U',X'00',AL3(USER)                                    
         DC    C'XTRACT   X',X'00',AL3(XTRACT)                                  
         DC    C'EXTRACT  X',X'00',AL3(XTRACT)                                  
         DC    C'FORMULA  Y',X'00',AL3(FORM)                                    
         DC    C'PROJECT  Y',X'00',AL3(FORM)                                    
         DC    C'NAME     0',X'80',AL3(NAME)                                    
         DC    C'CODE     0',X'80',AL3(CODE)                                    
         DC    X'FF'                                                            
         EJECT                                                                  
* WORK DSECT                                                                    
*                                                                               
WRKD     DSECT                                                                  
DUB      DS    D                                                                
WORK     DS    CL80                                                             
RELO     DS    A                                                                
ASYSLST  DS    A                                                                
BLOCK    DS    CL160                                                            
DMCB     DS    6F                                                               
SPACES   DS    CL80                                                             
P        DS    CL80                                                             
DATAMGR  DS    V                   EXTRNS                                       
GETMSG   DS    V                                                                
DATCON   DS    V                                                                
SCANNER  DS    V                                                                
SQUASHER DS    V                                                                
CHOPPER  DS    V                                                                
HEXOUT   DS    V                                                                
TERMINAL DS    CL1                                                              
USERID   DS    H                                                                
ERROR    DS    CL1                                                              
MYRE     DS    F                                                                
PARMS    DS    0CL24                                                            
AFACLIST DS    F                                                                
         DS    F                                                                
AUTL     DS    F                                                                
ACOMFACS DS    F                                                                
         DS    F                                                                
ATWA     DS    F                                                                
KEY      DS    CL25                                                             
KEYSAVE  DS    CL25                                                             
DDS      DS    X                                                                
ELCODE   DS    CL1                                                              
ARTN     DS    A                                                                
FILTAREA DS    0CL40                                                            
FILTAGY  DS    CL7                                                              
FILTID   DS    CL10                                                             
FILTPASS DS    CL10                                                             
FILTPROF DS    CL1                                                              
FILTIDNO DS    CL2                                                              
FILTOUT  DS    CL10                                                             
PAGE     DS    18CL80                                                           
IO       DS    2000C                                                            
IO2      DS    2000C                                                            
WRKX     EQU   *                                                                
         EJECT                                                                  
CTINFD   DSECT                                                                  
         DS    CL64                                                             
       ++INCLUDE CTINFFFD                                                       
LASTKEY  DS    CL25                                                             
LASTUSER DS    CL16                                                             
LASTCODE DS    CL9                                                              
         EJECT                                                                  
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* FAUTL                                                                         
         PRINT OFF                                                              
       ++INCLUDE FAUTL                                                          
         PRINT ON                                                               
         SPACE 1                                                                
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
         SPACE 1                                                                
* FASYSLSTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FASYSLSTD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'046CTINF00A  05/01/02'                                      
         END                                                                    
