*          DATA SET DDSENDMAIL AT LEVEL 009 AS OF 02/04/19                      
*PHASE SENMAILA                                                                 
*INCLUDE DMDMGRL                                                                
*INCLUDE CARDS                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE SCANNER                                                                
*INCLUDE SMTP                                                                   
*INCLUDE PRINT                                                                  
*INCLUDE KHDUMMY                                                                
         ENTRY SSB                                                              
         ENTRY UTL                                                              
         TITLE 'SEND EMAIL'                                                     
         PRINT NOGEN                                                            
SENDMAIL CSECT                                                                  
         NBASE 0,SENDMAIL,WORK=A(WORK),R8                                       
                                                                                
         BRAS  RE,VALPARM                                                       
         BRAS  RE,RUNCHK                                                        
         JNE   EXIT                                                             
         BRAS  RE,EMAIL                                                         
                                                                                
EXIT     XBASE                                                                  
XIT      XIT1                                                                   
         EJECT ,                                                                
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
         GOTOR =V(SCAN31),DMCB,C,(R2),0,('MAXPRMS',SCICARD),75                  
         SR    R0,R0                                                            
         ICM   R0,1,4(R1)          Number of parameters                         
         BZ    VALPNXT             None                                         
                                                                                
         USING SCANBLKD,R2                                                      
         USING PARMD,R3                                                         
         LA    R3,PARMTAB                                                       
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
         CLC   SC1STFLD,PARMFLD                                                 
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
                                                                                
**********************************************************************          
* Set DSPACE                                                                    
**********************************************************************          
         USING SSBD,RF                                                          
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
* To                                                                            
**********************************************************************          
TO       LARL  R2,SENDTO                                                        
         MVC   0(77,R2),C+3                                                     
         BR    RE                                                               
**********************************************************************          
* From                                                                          
**********************************************************************          
FROM     LARL  R2,FROMTXT                                                       
         MVC   0(75,R2),C+5                                                     
         BR    RE                                                               
**********************************************************************          
* Subject                                                                       
**********************************************************************          
SUBJECT  LARL  R2,SUBTEXT                                                       
         MVC   0(72,R2),C+8                                                     
         BR    RE                                                               
**********************************************************************          
* CC  - carbon copy                                                             
**********************************************************************          
CC       LARL  R2,CCTEXT                                                        
         MVC   0(77,R2),C+3                                                     
         BR    RE                                                               
**********************************************************************          
* BC  - blind copy                                                              
**********************************************************************          
BC       LARL  R2,BCTEXT                                                        
         MVC   0(77,R2),C+3                                                     
         BR    RE                                                               
**********************************************************************          
* Body                                                                          
**********************************************************************          
BODY     L     RF,BODYCNT                                                       
         AHI   RF,1                                                             
         ST    RF,BODYCNT                                                       
         BCTR  RF,0                                                             
         LARL  R2,BODYTXT                                                       
         MHI   RF,L'BODYTXT                                                     
         AR    R2,RF                                                            
         MVC   0(75,R2),C+5                                                     
         BR    RE                                                               
                                                                                
**********************************************************************          
* EMAIL                                                                         
**********************************************************************          
EMAIL    NTR1                                                                   
         GOTOR VSMTP,DMCB,('SMTPAINI',0)                                        
                                                                                
         LARL  R2,SENDTO                      P1                                
         LARL  R3,SUBTEXT                     P2                                
*                                                                               
         LARL  R4,CCTEXT                                                        
         ST    R4,DMCB+08                     P3                                
         MVI   DMCB+08,L'CCTEXT                                                 
         CLC   0(L'CCTEXT,R4),SPACES                                            
         BH    *+10                                                             
         XC    DMCB+08(4),DMCB+08                                               
*                                                                               
         LARL  R4,BCTEXT                                                        
         ST    R4,DMCB+12                     P4                                
         MVI   DMCB+12,L'BCTEXT                                                 
         CLC   0(L'BCTEXT,R4),SPACES                                            
         BH    *+10                                                             
         XC    DMCB+12(4),DMCB+12                                               
*                                                                               
*        GOTOR VSMTP,DMCB,('SMTPAPRS',(R2)),(L'SUBTEXT,(R3))                    
         GOTOR VSMTP,DMCB,('SMTPATCS',(R2)),(L'SUBTEXT,(R3)),,,0,0              
                                                                                
         L     R0,BODYCNT                                                       
         LARL  R2,BODYTXT                                                       
EMAIL10  GOTOR VSMTP,DMCB,('SMTPAPTL',(R2))                                     
         LA    R2,L'BODYTXT(,R2)                                                
         JCT   R0,EMAIL10                                                       
                                                                                
         GOTOR VSMTP,DMCB,('SMTPASND',0)                                        
         GOTOR VSMTP,DMCB,('SMTPAEND',0)                                        
         J     EXIT                                                             
**********************************************************************          
* Valid parameters                                                              
**********************************************************************          
PARMTAB  DS    0A                                                               
         DC    C'DSPACE  ',AL1(6,SGL,1,RTN),A(XDSPACE,DSPACE)                   
         DC    C'DDSIO   ',AL1(5,SGL,8,MVC+VAR),A(0),V(DDSIO)                   
         DC    C'TO      ',AL1(2,SGL,0,RTN),A(TO,0)                             
         DC    C'FROM    ',AL1(4,SGL,0,RTN),A(FROM,0)                           
         DC    C'CC      ',AL1(2,SGL,0,RTN),A(CC,0)                             
         DC    C'BC      ',AL1(2,SGL,0,RTN),A(BC,0)                             
         DC    C'SUBJECT ',AL1(7,SGL,0,RTN),A(SUBJECT,0)                        
         DC    C'BODY    ',AL1(4,0,0,RTN),A(BODY,0)                             
         DC    AL1(EOT)                                                         
         EJECT ,                                                                
                                                                                
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
         BE    RUNGOOD                                                          
         AHI   R2,1                                                             
         AHI   R3,2                                                             
         BRCT  R1,RUNCHK12                                                      
         WTO   'INVALID OR NOT SUPPORTED DSPACE VALUE'                          
         J     RUNBAD                                                           
                                                                                
         USING SSBD,RF                                                          
RUNGOOD  L     RF,=A(SSB)                                                       
         OI    SSOMTIND,SSOWRTN                                                 
         DROP  RF                                                               
                                                                                
         SR    RE,RE                                                            
RUNBAD   LTR   RE,RE                                                            
         J     XIT                                                              
**********************************************************************          
*                                                                               
**********************************************************************          
ONLY     EQU   C'O'                                                             
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
K        EQU   1024                                                             
EOR      EQU   0                                                                
EOT      EQU   X'FF'               End of table                                 
FF       EQU   X'FF'                                                            
MAXPRMS  EQU   120                                                              
MAXFILEQ EQU   6                                                                
ERROR#   DS    X                                                                
                                                                                
DMWORK   EQU   *                                                                
VDSPACE  DC    C'ACQT'                                                          
VUPDID   DC    C'DMFCFQFT'                                                      
LIST     DC    (MAXFILEQ)CL8' '                                                 
         DC    C'X'                                                             
MAXLISTQ EQU   (MAXFILEQ*L'LIST)                                                
COMMAND  DC    CL8' '                                                           
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
CTFILE   DC    CL8'CTFILE'                                                      
DMADD    DC    CL8'ADDREC'                                                      
DADDS    DC    CL8'DADDS'                                                       
                                                                                
RECEIPT1 DC    CL80'ANTHONY.HYDE@DDS.NET:'                                      
SUBJECT1 DC    CL80'TESTING 1 2 3'                                              
TEXT1    DC    CL80'Test USS queue to SMTP'                                     
BODYCNT  DC    F'0'                                                             
VSMTP    DC    V(SMTP)                                                          
HIBLOCK  DS    A                                                                
AIO      DC    A(IO)                                                            
SVRE     DS    A                                                                
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
FULL     DC    F'0'                                                             
MSG      DS    CL20                                                             
*                                                                               
DSPACE   DS    C                                                                
*                                                                               
C        DC    CL80' '                                                          
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
                                                                                
         LTORG                                                                  
                                                                                
IO       DS    CL(2*K)                                                          
                                                                                
WORK     DC    5000D'0'                                                         
**********************************************************************          
*                                                                               
**********************************************************************          
SENDTO   DC    CL80' '                                                          
SUBTEXT  DC    CL80' '                                                          
FROMTXT  DC    CL80' '                                                          
CCTEXT   DC    CL80' '                                                          
BCTEXT   DC    CL80' '                                                          
BODYTXT  DC    10000CL80' '                                                     
**********************************************************************          
*                                                                               
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
HEX      EQU   X'20'               Hex number input                             
NUM      EQU   X'10'               Number (FIGNUM) Full word value              
MVC      EQU   X'02'               Move character for length n                  
VAR      EQU   X'01'               Variable length                              
PARMRTN  DS    A                                                                
PARMVAR  DS    A                                                                
PARMLNQ  EQU   *-PARMD                                                          
                                                                                
* SCANBLKD                                                                      
       ++INCLUDE DDSCANBLKD                                                     
                                                                                
* SMTPD                                                                         
       ++INCLUDE DDSMTPD                                                        
*************************                                                       
*        DMGREQUS       *                                                       
*************************                                                       
       ++INCLUDE DMGREQUS                                                       
                                                                                
       ++INCLUDE FASSB                                                          
         ORG SSBD                                                               
       ++INCLUDE FASSBOFF                                                       
         ORG                                                                    
*UTLD                                                                           
       ++INCLUDE FAUTL                                                          
*                                                                               
*ACGENFILE                                                                      
*        PRINT OFF                                                              
*      ++INCLUDE ACGENFILE                                                      
*        PRINT ON                                                               
         IHAOUXB DSECT=YES                                                      
         IHAASCB LIST=YES                                                       
         IAZJSAB LIST=YES                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009DDSENDMAIL02/04/19'                                      
         END                                                                    
