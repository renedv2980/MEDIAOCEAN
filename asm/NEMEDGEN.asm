*          DATA SET NEMEDGEN   AT LEVEL 010 AS OF 11/09/18                      
*PHASE T00A42B,+0                                                               
*INCLUDE GETNUN                                                                 
*INCLUDE PERVERT                                                                
************************ MACROS  **********************************             
         MACRO                                                                  
&TAG     IFMVC &A,&B,&C                                                         
&TAG     LTR   RF,&A                                                            
         BZ    IFM&SYSNDX                                                       
         LA    R1,&B                                                            
         MOVE  ((RF),(R1)),&C                                                   
IFM&SYSNDX   EQU   *                                                            
         MEND                                                                   
*                                                                               
T00A42   TITLE '-   NETWORK GENERAL ROUTINES'                                   
* ORGANIZATION OF MEMORY: USED BY ENTIRE NETWORK SYSTEM                         
*                                                                               
*      20000 BYTES                                                              
*      '-------------'                                                          
* RC-> ' GEND        '                                                          
*      ' (2792 BYTES)'                                                          
*      '-------------'                                                          
*      ' IO AREA     '                                                          
*      ' (2000 BYTES)'                                                          
*      '-------------'                                                          
* R9-> ' NETSYSD     '                                                          
*      '  (424 BYTES)'                                                          
*      '-------------'                                                          
*      ' NETBLOCK    '                                                          
*      ' (1024 BYTES)'                                                          
*      '-------------'                                                          
*      ' DRIVEBLOCK  '                                                          
*      ' (1280 BYTES)'                                                          
*      '-------------'                                                          
*      ' DRONEBLOCK  '                                                          
*      ' (580 BYTES) '                                                          
*      '-------------'                                                          
*      ' APPLICATION '                                                          
*      '   COMMON    '                                                          
*      '  (W/S 1)    '                                                          
*      ' (2008 BYTES)                                                           
*      '-------------'                                                          
*      ' LOCAL W/S   '                                                          
*      '   (W/S 2)   '                                                          
*      '-------------'                                                          
************************************************************                    
         EJECT                                                                  
T00A42   CSECT                                                                  
         DS    15000C              (3A98 HEX)                                   
         ORG   *-15000                                                          
         PRINT NOGEN                                                            
         REQUS                                                                  
         USING *,RF                                                             
NGEN     NTR1                                                                   
         LA    RB,NGEN                                                          
         DROP  RF                                                               
         LA    R7,2048(RB)                                                      
         LA    R7,2048(R7)                                                      
         LA    R8,2048(R7)                                                      
         LA    R8,2048(R8)                                                      
         LA    RA,2048(R8)                                                      
         LA    RA,2048(RA)                                                      
         USING T00A42,RB,R7,R8,RA                                               
***      LA    R3,RELOC                                                         
***      S     R3,RELOC                                                         
***      ST    R3,RELO                                                          
         USING GEND,RC                                                          
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         SRL   RF,24               GET ROUTINE NUMBER                           
         SLL   RF,2                MULTIPLY BY 4                                
         B     VBRANCH(RF)                                                      
         SPACE 1                                                                
RELOC    DC    A(*)                                                             
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              BRANCH TABLE                                                     
         SPACE 3                                                                
*                                  ORDERED AS IN NENVEQUS                       
         SPACE 1                                                                
VBRANCH  B     VVERR               0                                            
         B     VVGETFLD            1                                            
         B     VVAGYOUT            2                                            
         B     VVERR               3  (VVAGYALL)                                
         B     VVAGY               4                                            
         B     VVCLIOUT            5                                            
         B     VVCLIALL            6                                            
         B     VVCLI               7                                            
         B     VVPRDOUT            8                                            
         B     VVPRDALL            9                                            
         B     VVPRD               10                                           
         B     VVESTOUT            11                                           
         B     VVESTALL            12                                           
         B     VVEST               13                                           
         B     VVPAKOUT            14                                           
         B     VVPAK               15                                           
         B     VVNETALL            16                                           
         B     VVNET               17                                           
         B     VVDEM               18                                           
         B     VVDPT               19                                           
         B     VVGETDEM            20                                           
         B     VVPRDEM             21                                           
         B     VVSTRDAT            22                                           
         B     VVENDDAT            23                                           
         B     VVDELHOM            24                                           
         B     VVESTRNG            25                                           
         B     VVPAKLOK            26                                           
         B     VVWKLST             27                                           
         B     VVWKLBEF            28                                           
         B     VVDAY               29                                           
         B     VVSETUNT            30                                           
         B     VVSETSPT            31                                           
         B     VVSETSTA            32                                           
         B     VVSETCT             33                                           
         B     VVFILT              34                                           
         B     VVDEMCON            35                                           
         B     VVDEMTYP            36                                           
         B     VVDPTALL            37                                           
         B     VVEFILT             38                                           
         B     VVACFLT             39                                           
         B     VVEFOUT             40                                           
         B     VVPSEQ              41                                           
         B     VVDEMOPT            42                                           
         B     VVUNIV              43                                           
         B     VVPFILT             44                                           
         B     VVPFOUT             45                                           
         B     VVPRCPM             46                                           
         B     VVHEAD              47                                           
         B     VVDRINIT            48                                           
         B     VVSTRBIL            49                                           
         B     VVENDBIL            50                                           
         B     VVSTRPAY            51                                           
         B     VVENDPAY            52                                           
         B     VVCURSOR            53                                           
         B     VVTARBUF            54                                           
         B     VVTITLE             55                                           
         B     VVTITOUT            56                                           
         B     VVACNEW             57                                           
         B     VVDATA              58                                           
         B     VVPRDPGE            59                                           
         B     VVUSERID            60                                           
         B     VVBDACTY            61                                           
         B     VVCHKRSN            62                                           
         SPACE 1                                                                
VCOUNT   EQU   (*-VBRANCH)/4                                                    
VVERR    DC    H'0'                                                             
         EJECT                                                                  
*              GETFLD ROUTINE - EXTRACT DATA FROM SCREEN FIELD                  
         SPACE 3                                                                
*              INPUTS              R2=A(FIELD HEADER)                           
*                                  FTERMFLG=1 FIELD IS OPTIONAL                 
*              ARGUMENTS           1 (R5) MAXIMUM NUMERIC VALUE                 
*                                         DEFAULT IS 255                        
*              LOCAL               R4=MAX NUMERIC VALUE                         
*              OUTPUTS             FLDH  CONTAINS FIELD HEADER                  
*                                  FLD   FIELD DATA SPACE FILLED                
*                                  R0    BINARY VALUE IF FIELD NUMERIC          
*                                  R1    FIELD LENGTH                           
*                                  CONDITION CODE ZERO IF R1=0                  
         SPACE 1                                                                
VVGETFLD L     R5,0(R1)            GET A(FULLWORD)                              
         LA    R4,255              SET R4 TO DEFAULT IN CASE R5=0               
         LTR   R5,R5               IF ZERO THEN                                 
         BZ    *+8                                                              
         L     R4,0(R5)            GET ACTUAL VALUE IN R4                       
         MVC   FLDH,0(R2)                                                       
         MVI   FLD,C' '                                                         
         MVC   FLD+1(L'FLD-1),FLD  FILL WITH BLANKS                             
         SR    R0,R0               PRE-CLEAR REGISTER FOR NUMERIC VALUE         
         ZIC   R1,FLDH                                                          
         SHI   R1,9                                                             
         TM    FLDH+1,X'02'        TEST FOR EXTENDED HEADER                     
         BNO   GETFLD0                                                          
         SHI   R1,8                (SUBTRACT 8 MORE)                            
GETFLD0  EX    R1,FLDMOVE                                                       
         LA    RE,FLD(R1)          POINT RE AT LAST EXTRACTED BYTE              
         LA    R1,1(R1)            RESTORE DATA LENGTH                          
         SPACE 1                                                                
GETFLD1  CLI   0(RE),C' '          FIND ACTUAL DATA LENGTH                      
         BH    GETFLD2                                                          
         MVI   0(RE),C' '          MAKE SURE BINARY ZERO GOES TO BLANK          
         BCTR  RE,0                                                             
         BCT   R1,GETFLD1                                                       
         SPACE 1                                                                
GETFLD2  STC   R1,FLDH+5           SET ACTUAL DATA LENGTH                       
         LTR   R1,R1               TEST FOR EMPTY FIELD                         
         BZ    GTFERRCK                                                         
         SPACE 1                                                                
GETFLD4  LR    R3,R1               GET FLD LENGTH-1 IN R3                       
         BCTR  R3,0                                                             
**       MVC   WORK(6),=6X'F0'     TEST FOR NUMERIC FIELD                       
**       EX    R3,MOVEZONE                                                      
**       CLC   WORK(6),=6X'F0'                                                  
**       BNE   GETFLDX                                                          
         EX    R3,MOVEZERO         TEST FOR NUMERIC FIELD                       
         EX    R3,MOVEZONE                                                      
         EX    R3,CLCZERO                                                       
         BNE   GETFLDX                                                          
         EX    R3,FLDPACK                                                       
         CVB   R0,DUB                                                           
         LTR   R0,R0               CK IF INPUT=0                                
         BZ    GTNOTNUM                                                         
         CR    R0,R4               CK IF GT SPECIFIED MAXIMUM                   
         BH    GTNOTNUM                                                         
         B     GETFLDX                                                          
         SPACE 1                                                                
GTNOTNUM SR    R0,R0               NON-NUMERIC. SO SET R0 TO 0                  
         B     GETFLDX                                                          
         SPACE 1                                                                
GTFERRCK CLI   FTERMFLG,1          IS THIS OK?                                  
         BE    GETFLDX                                                          
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
         SPACE 1                                                                
GETFLDX  LTR   R1,R1                                                            
         XIT1  REGS=(R0,R1)                                                     
         SPACE 1                                                                
FLDMOVE  MVC   FLD(0),8(R2)                                                     
         SPACE 1                                                                
FLDPACK  PACK  DUB,FLD(0)                                                       
         SPACE 1                                                                
MOVEZONE MVZ   WORK(0),8(R2)                                                    
         SPACE 1                                                                
MOVEZERO MVC   WORK(0),=10X'F0'                                                 
         SPACE 1                                                                
CLCZERO  CLC   WORK(0),=10X'F0'                                                 
         SPACE 1                                                                
         EJECT                                                                  
*              AGENCY AND MEDIA ROUTINES                                        
         SPACE 3                                                                
VVAGYOUT DS    0H                                                               
*--MAKE SURE SYSTEM SET TO NET                                                  
*                                                                               
         CLI   MODE,NEWSCR          NEW SCREEN JUST LOADED                      
         BNE   *+8                                                              
         BRAS  RE,CHKSCRN           SEE IF ANYTHING SPECIAL NEEDED              
*                                                                               
         LA    R2,=C'NET'                                                       
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         LTR   RF,RF               ONLY USED IF ONLINE                          
         BZ    VVA1                                                             
*                                                                               
         GOTO1 (RF),DMCB,(R2),0                                                 
         MVC   SYSDIR,0(R2)                                                     
         MVC   SYSFIL,0(R2)                                                     
*                                                                               
VVA1     MVC   NBACOM,ACOMFACS     SET UP NETBLOCK                              
         MVC   NBAIO,AIO                                                        
         MVI   NBQINIT,0           RE-INITIALIZE                                
         L     R1,ATWA                                                          
         USING TWADSEC,R1                                                       
         MVC   NBNOWRIT,TWAWRITE   ALLOWS WRITE TO BE INHIBITED                 
         DROP  R1                                                               
         SPACE 1                                                                
         GOTO1 CALLOV,DMCB,0,X'D9000A39'   LOAD T00A39 (DRONE)                  
         L     R2,DMCB                                                          
         ST    R2,NDDRONE                                                       
         SPACE 1                                                                
         CLI   OFFLINE,C'Y'        ALSO INHIBIT WRITE IF OFFLINE                
         BNE   VVA2                                                             
         MVI   NBNOWRIT,C'N'                                                    
         SPACE 1                                                                
VVA2     MVI   NBSELMOD,NBVALAGY                                                
         MVC   NBSELAGY,AGENCY                                                  
***      LA    R1,NDPRGBUF         PASS BUFFER ADDRESSES TO NETIO               
***      ST    R1,NBAPBUFF         *APPS WILL NOW DO THIS 4/25/04               
         NETGO NSNETIO,DMCB,NETBLOCK                                            
         LA    R1,NETBLOCK                                                      
         ST    R1,NDNETBLK                                                      
         MVC   NDAGYABR,AGENCY                                                  
         MVC   NDAGYKEY,NBACTAM                                                 
         MVI   NBSELMOD,0          RESET SELECT MODE                            
         MVC   ERROR,NBERROR       RETURNED                                     
         CLI   NBERROR,NBGOOD                                                   
         BNE   TRAPERR                                                          
         L     R4,NBAIO                                                         
         USING AGYHDR,R4                                                        
         L     R1,ASPOOLD                                                       
         USING SPOOLD,R1                                                        
         MVC   USERNAME,AGYNAME                                                 
         MVC   USERADDR,AGYADDR                                                 
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BE    NOSOXCHK                                                         
         L     R3,SYSPARMS                                                      
         L     RE,16(R3)           A(COMFACS)                                   
         L     RF,CXTRAINF-COMFACSD(,RE)                                        
         MVC   BYTE,XIFLAG1-XTRAINFO(RF)                                        
         TM    BYTE,XIROSYS+XIROMODE+XIWRONGF  READ ONLY SYSTEM/MODE            
         BZ    *+8                                                              
         OI    NBINDS6,NBI6SOX     NO UPDATE                                    
NOSOXCHK EQU   *                                                                
*                                                                               
         TM    AGYFLAG2,AGYFLAG2_2DP+AGYFLAG2_BDP   2 DEC PREC?                 
         BZ    NOT2DEC                                                          
         OI    NBINDS3,NBI3A2DC        ,,YES                                    
NOT2DEC  DS    0H                                                               
*                                  AGENCY NAME & ADDRESS FROM ID REC.           
         MVC   FILENAME,=CL8'CTFILE'                                            
         MVI   USEIO,C'Y'                                                       
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTIKEY,R4                                                        
         MVI   CTIKTYP,C'I'                                                     
         L     RF,ATWA                                                          
         MVC   CTIKID+8(2),10(RF)                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         L     R4,AIO                                                           
         LA    R4,CTIDATA                                                       
         SR    R3,R3                                                            
         SPACE 1                                                                
VUSER1   CLI   0(R4),0                                                          
         BE    VUSER3                                                           
         CLI   0(R4),X'36'                                                      
         BE    VUSER2A                                                          
         SPACE 1                                                                
VUSER1A  IC    R3,1(R4)                                                         
         AR    R4,R3                                                            
         B     VUSER1                                                           
         SPACE 1                                                                
         USING CTORGD,R4                                                        
VUSER2A  L     R1,ASPOOLD                                                       
         USING SPOOLD,R1                                                        
         MVC   USERNAME,CTORGNAM                                                
         MVC   USERADDR,CTORGADD                                                
         B     VUSER1A                                                          
         SPACE 1                                                                
VUSER3   XC    FILENAME,FILENAME                                                
         MVI   USEIO,0                                                          
         XC    NBSECSV,NBSECSV         CLEAR NEW SECURITY                       
         BRAS  RE,INITSEC              FOR NOW - ONLY ONLINE                    
         B     XIT                                                              
         DROP  R1                                                               
         DROP  R4                                                               
         EJECT                                                                  
*              STANDARD VALIDATE AGENCY                                         
         SPACE 3                                                                
*              ARGUMENTS           1 (R3) A(OUTPUT AGENCY NAME)                 
*                                  2 (R4) A(OUTPUT AGENCY ADDRESS)              
         SPACE 1                                                                
VVAGY    LM    R3,R4,0(R1)         SAVE ARG LIST                                
         MVI   NBQINIT,0           START AT BEGINNING                           
         MVI   NBSELMOD,NBVALAGY                                                
         NETGO NSNETIO,DMCB,NETBLOCK   VALIDATE AGY                             
         MVI   NBSELMOD,0          RESET SELECT MODE                            
         MVC   ERROR,NBERROR       RETURNED                                     
         CLI   NBERROR,NBGOOD                                                   
         BNE   TRAPERR                                                          
         L     R6,NBAIO            AGY RECORD                                   
         USING AGYHDR,R6                                                        
         IFMVC R3,L'AGYNAME,AGYNAME     MOVE IF ARG 1 GIVEN                     
         IFMVC R4,L'AGYADDR,AGYADDR     MOVE IF ARG 2 GIVEN                     
         MVC   NDAGYABR,AGENCY                                                  
         MVC   NDAGYKEY,NBACTAM                                                 
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*              ROUTINE TO OUTPUT CLIENT                                         
         SPACE 3                                                                
*                                  ASSUMES AGY IN NETBLOCK                      
*              INPUT               R2=A(CLIENT FIELD)                           
*              LOCAL               R3=A(TWA)                                    
         SPACE 1                                                                
VVCLIOUT L     R3,ATWA                                                          
         USING TWADSEC,R3                                                       
         MVI   NBSELMOD,NBVALCLI                                                
         MVC   NBTWAACC,TWAACCS                                                 
         NETGO NSNETIO,DMCB,NETBLOCK                                            
         MVI   NBSELMOD,0          RESET SELECT MODE                            
         MVC   ERROR,NBERROR       RETURNED                                     
         CLI   NBERROR,NBGOOD                                                   
         BNE   TRAPERR                                                          
         CLI   NBMODE,NBREQLST                                                  
         BNE   VVC2                                                             
         MVI   ERROR,INVOFF        NO CLIENTS                                   
         B     TRAPERR                                                          
         SPACE 1                                                                
VVC2     CLI   OFFLINE,C'Y'            IF OFFLINE                               
         BE    VVCLIXIT                NETIO FILTERS CLIENTS                    
         L     R4,NBAIO                                                         
         USING CLTHDR,R4                                                        
         TM    NBUNTSW,X'20'       SKIP ACCESS CHECKS                           
         BO    VVCLIXIT                                                         
         OC    TWAACCS-2(4),TWAACCS-2  NEW+OLD SECURITY?                        
         BZ    VVCLIXIT                                                         
         OC    NBSECSV,NBSECSV         NEW SECURITY AREA PASSED?                
         BNZ   VVCLILST                YES/DO NEW SECURITY                      
*                                      NO/CONTINUE OLD SECURITY                 
         OC    TWAACCS(2),TWAACCS  TWA TELLS IF LOCKOUT CHECKS                  
         BZ    VVCLIXIT                                                         
         CLI   TWAACCS,C'*'        IF * THEN SKIP CLI LOCKOUT                   
         BE    VVCLIOFF                                                         
         CLI   TWAACCS,C'$'        IF $ THEN SKIP CLI LOCKOUT                   
         BE    VVCLILST                                                         
         CLI   TWAACCS,C'-'        IF NOT * OR - THEN CLIENT LOCKOUT            
         BNE   VVCLICLI                                                         
         CLI   TWAACCS+1,C'*'      -* MEANS ALL BUT THIS OFFICE                 
         B     VVCLXOFF                                                         
         SPACE 1                                                                
VVCLXCLI CLC   TWAACCS+1(2),NBACTCLI   ALL BUT THIS CLIENT                      
         BE    VVCLIERR                                                         
         B     VVCLIXIT                                                         
         SPACE 1                                                                
VVCLICLI CLC   TWAACCS(2),NBACTCLI     CLIENT LOCKOUT                           
         BE    VVCLIXIT                                                         
         B     VVCLIERR                                                         
         SPACE 1                                                                
VVCLIOFF DS    0H                 CLIENT GRP LIMIT ACCESS                       
         CLI   TWAACCS+1,C'A'        CHECK IF ALPHA                             
         BL    VVCLIOF5                                                         
         CLI   TWAACCS+1,C'Z'                                                   
         BH    VVCLIOF5                                                         
         CLI   TWAACCS+2,C'0'         CHECK IF NUMERIC                          
         BL    VVCLIOF5                                                         
         CLI   TWAACCS+2,C'9'                                                   
         BH    VVCLIOF5                                                         
         BAS   RE,VALGROUP            VALIDATE GROUP NUMBER                     
         B     VVCLIXIT                                                         
*                                                                               
VVCLIOF5 CLC   TWAACCS+1(1),NBEFFOFF   OFFICE LOCKOUT                           
         BE    VVCLIXIT                                                         
         B     VVCLIERR                                                         
         SPACE 1                                                                
VVCLILST ICM   R3,15,NBSECSV          NEW SECURITY ?                            
         BNZ   VNEWSEC                 YES                                      
         LA    R1,DUB                   NO                                      
         XC    DUB,DUB                                                          
         USING OFFICED,R1                                                       
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAUTH,TWAACCS                                                  
         MVC   OFCLMT,TWAACCS                                                   
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC,NBEFFOFF                                                  
         DROP  R1                                                               
         GOTO1 AOFFICER,DMCB,DUB,ACOMFACS,0                                     
         CLI   0(R1),0                                                          
         BE    VVCLIXIT                                                         
         B     VVCLIERR                                                         
                                                                                
**** NEW SECURITY CALL ****                                                     
VNEWSEC  DS    0H                                                               
         BRAS  RE,SECRTN                                                        
         BNE   VVCLIERR                                                         
         B     VVCLIXIT                                                         
*                                                                               
**********************                                                          
         SPACE 1                                                                
VVCLXOFF CLC   TWAACCS+2(1),NBEFFOFF   ALL BUT THIS OFFICE                      
         BE    VVCLIERR                                                         
         B     VVCLIXIT                                                         
         SPACE 1                                                                
VVCLIERR MVI   ERROR,249        SECURITY LOCKOUT                                
*VVCLIERR MVI   ERROR,INVCLI                                                    
         B     TRAPERR                                                          
         SPACE 1                                                                
VVCLIXIT B     XIT                                                              
         DROP  R4                                                               
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
VALGROUP NTR1                                                                   
         L     R3,ATWA                                                          
         USING TWADSEC,R3                                                       
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CLGRECD,R4          CLIENT STATION PASSIVE POINTER               
*                                                                               
         MVC   0(2,R4),=X'0D86'       RECORD TYPE                               
         MVC   CLGCAGMD,NBACTAM       AGY/MED                                   
         MVC   CLGCID,TWAACCS+1       GROUP ID                                  
         MVC   FULL,BLANKS                                                      
         MVC   FULL(2),TWAACCS+2   GROUP CODE                                   
         OC    FULL,=C'0000'       REPLACE BLANKS WITH X'F0'                    
         PACK  DUB,FULL                                                         
         L     R0,DUB+4                                                         
         SRL   R0,4                GET RID OF SIGN NIBLE                        
         STCM  R0,3,CLGCGRP        LEFT-JUSTIFIED, PWOS                         
         MVC   CLGCCLT,NBACTCLI       CLIENT                                    
*                                                                               
         MVC   FILENAME,=CL8'SPTDIR'                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    VALGR20                                                          
****     MVI   ERROR,INVCLI        NO MATCH /ERROR                              
****     B     TRAPERR                                                          
         B     VVCLIERR                                                         
*                                                                               
VALGR20  XC    KEY,KEY                                                          
         XC    FILENAME,FILENAME                                                
         B     XIT                                                              
         DROP  R4,R3                                                            
         EJECT                                                                  
*              STANDARD VALIDATE CLIENT                                         
         SPACE 3                                                                
*              ARGUMENTS           1  A(OUTPUT CLIENT NAME)                     
*                                  2  A(OUTPUT PRODLIST)                        
         SPACE 1                                                                
VVCLI    LM    R3,R4,0(R1)         ARG LIST                                     
         XC    NBSELCLI,NBSELCLI   CLEAR TO DEFAULTS                            
         ST    R2,NDCLISCR         SAVE ADDRESS OF SCREEN HEADER                
         MVI   NDCLIRMD,NDTOG                                                   
         NETGO NVGETFLD,DMCB                                                    
         BZ    XITVCLI             IF NO INPUT                                  
         MVC   NBSELCLI,FLD                                                     
         CLC   NBSELCLI,=C'ALL'                                                 
         BNE   LVC2                                                             
*****    MVI   ERROR,INVCLI        DONT ALLOW ALL                               
*****    B     TRAPERR                                                          
         B     VVCLIERR                                                         
         SPACE 1                                                                
LVC2     NETGO NVCLIOUT,DMCB       FILL STANDARD STUFF                          
         MVI   NDCLIRMD,NDONE                                                   
         L     R6,NBAIO            CLIENT RECORD                                
         USING CLTHDR,R6                                                        
         IFMVC R3,L'CNAME,CNAME      MOVE IF FIRST ARG GIVEN                    
         IFMVC R4,880,CLIST          MOVE IF SECOND ARG GIVEN                   
         SPACE 1                                                                
XITVCLI  B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*              VALIDATE CLIENT - SPECIAL CASE=ALL                               
         SPACE 3                                                                
*              ARGUMENTS           1  A(OUTPUT CLIENT NAME)                     
*                                  2  A(OUTPUT PRODLIST)                        
*                                  SPACES RETURNED IF CLI=ALL                   
*              OUTPUT              NBSELCLI                                     
         SPACE 1                                                                
VVCLIALL LM    R3,R4,0(R1)         SAVE ARG LIST                                
         MVI   NDCLIRMD,NDTOG                                                   
         ST    R2,NDCLISCR         SAVE ADDRESS OF SCREEN HEADER                
         MVC   NBSELCLI,=C'ALL'    ASSUME ITS ALL FOR NETIO                     
         NETGO NVGETFLD,DMCB                                                    
         BZ    LVCALL              TAKE NO INPUT AS TOG                         
*                                                                               
         CLI   FLD+1,C'='         CLIENT GROUP FILTER (V=NNN)                   
         BE    VVCGR5             ONE CHARACTER                                 
         CLI   FLD+2,C'='         CLIENT GROUP FILTER (VV=NNN)                  
         BNE   VVCLIAA                                                          
         BRAS  RE,TRANS21          TRANSLATE 2 -> 1                             
         CLI   ERROR,INVALID                                                    
         BE    TRAPERR                                                          
         MVC   NBSELCGR(1),FLD         SET SCHEME                               
         CLC   =C'ALL',FLD+3                                                    
         BE    *+10                                                             
         MVC   NBSELCGR+1(4),FLD+3     AND NNNN                                 
         B     VVCGR7                                                           
*                                                                               
VVCGR5   MVC   NBSELCGR(1),FLD         SET SCHEME                               
         CLC   =C'ALL',FLD+2                                                    
         BE    *+10                                                             
         MVC   NBSELCGR+1(4),FLD+2     AND NNNN                                 
VVCGR7   NETGO NVCLIOUT,DMCB                                                    
         MVI   NDCLIRMD,NDONE                                                   
         B     LVCAXIT                                                          
*                                                                               
VVCLIAA  CLC   FLD(3),=C'NO,'                                                   
         BNE   VVCLIA1             NO,*O IS TO WITH OFFICE FILTER               
         LA    R5,FLD+3                                                         
         BAS   RE,DOOFF            SET OFFICE FILTERS                           
         B     LVCALL                                                           
         SPACE 1                                                                
VVCLIA1  MVI   NDCLIRMD,NDSEP                                                   
         CLI   FLD,C'*'            IF REQUEST BY OFFICE                         
         BE    VVCLIA2                                                          
         CLI   FLD,C'$'            IF REQUEST BY $                              
         BE    VVCLIA2                                                          
         B     VVCLA2                                                           
VVCLIA2  CLI   FLDH+5,1                                                         
         BNH   VVCLIERR                                                         
         CLI   FLDH+5,3                                                         
         BH    VVCLIERR                                                         
         LA    R5,FLD                                                           
         BAS   RE,DOOFF            SET OFFICE FILTER                            
         B     LVCALL                                                           
         SPACE 1                                                                
VVCLA2   MVC   NBSELCLI,FLD                                                     
         NETGO NVCLIOUT,DMCB       FILL STANDARD STUFF                          
         CLC   FLD(3),=C'ALL'      IF ALL GIVEN                                 
         BNE   VVCLA4                                                           
         CLI   FLD+3,C','          ALL,X IS ACCEPTABLE AS OFFICE                
         BNE   LVCALL                                                           
         LA    R5,FLD+4                                                         
         BAS   RE,DOOFF                                                         
         B     LVCALL                THEN DO FOR ALL                            
         SPACE 1                                                                
VVCLA4   MVI   NDCLIRMD,NDONE                                                   
         L     R6,NBAIO            CLIENT RECORD                                
         USING CLTHDR,R6                                                        
         IFMVC R3,L'CNAME,CNAME   MOVE IF FIRST ARG GIVEN                       
         IFMVC R4,880,CLIST       MOVE IF SECOND ARG GIVEN                      
         B     LVCAXIT                                                          
         SPACE 1                                                                
LVCALL   IFMVC R3,L'CNAME,BLANKS  MOVE SPACES IF FIRST ARG GIVEN                
         LTR   R4,R4               ARG GIVEN?                                   
         BZ    LVCAXIT             YES. THEN PROGRAM LOGIC ERROR.               
         MVI   ERROR,NBSYSERR                                                   
         B     TRAPERR                                                          
         SPACE 1                                                                
DOOFF    NTR1                      SET OFFICE FILTER                            
*                                  R5=A(FILTER EXPRESSION)                      
         L     R3,ATWA                                                          
         USING TWADSEC,R3                                                       
         MVC   NBTWAACC,TWAACCS                                                 
         CLI   0(R5),C'$'          OFFICE LIST EXPRESSION                       
         BE    DOOFF2                                                           
                                                                                
         CLI   0(R5),C'*'          GET PAST * UNLESS FORGOTTEN                  
         BNE   *+8                                                              
         LA    R5,1(R5)                                                         
         CLI   0(R5),C'-'          MINUS EXPRESSION                             
         BNE   *+12                                                             
         OI    NBOFFTYP,X'40'      INDICATE NEGATIVE FILTER                     
         LA    R5,1(R5)            AND BUMP OVER IT                             
*          DATA SET NEMEDGEN   AT LEVEL 066 AS OF 01/25/07 *********            
**********************************                                              
         LA    R4,DUB                                                           
         USING OFFICED,R4                                                       
         XC    0(OFCLENQ,R4),0(R4)                                              
         MVI   OFCSYS,C'S'                                                      
*********************************                                               
         ICM   RF,15,NBSECSV                                                    
         BZ    NOSECLST           NO SECURITY                                   
         USING SECD,RF                                                          
         OC    SECCLAL,SECCLAL     CLIENT STRING SECURITY?                      
         BNZ   SECLST              YES-JUST CONVERT OFFICE                      
         DROP  RF                  NO- PASS LIMIT ACCESS AND CONVERT            
NOSECLST EQU   *                                                                
         MVC   OFCAUTH,TWAACCS     SET LIMIT ACCESS                             
         MVC   OFCLMT,TWAACCS                                                   
SECLST   DS    0H                                                               
         MVC   OFCAGY,AGENCY                                                    
         CLI   1(R5),0            **IF ZERO                                     
         BNE   *+8                                                              
         MVI   1(R5),X'40'        **MAKE 2ND POSITION BLANK                     
         MVC   OFCOFC2,0(R5)                                                    
         GOTO1 AOFFICER,DMCB,(C'2',OFFICED),(0,ACOMFACS)                        
         CLI   0(R1),0                                                          
         BE    OFFICEOK                                                         
         MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
OFFICEOK MVC   NBSELOFF,OFCOFC      SET 1 BYTE INTERNAL OFFICE CODE             
         DROP  R4                                                               
DOOFF1D  CLI   TWAACCS,C'*'        IS THERE LIMIT ACCESS                        
         BE    DOOFF1DA               YES OK                                    
         B     XIT                                                              
*          DATA SET NEMEDGEN   AT LEVEL 066 AS OF 01/25/07************          
***      CLI   TWAACCS,C'$'           IF $ OK                                   
***      BE    XIT                                                              
***      CLI   TWAACCS,C'-'           IF NEGATIVE FILTERING, OK                 
***      BE    XIT                                                              
***      CLI   TWAACCS,0              OR OPEN, OK                               
***      BE    XIT                                                              
***      B     VVCLIERR            ELSE ERROR                                   
*                                                                               
*                                                                               
*        LA    R4,DUB                                                           
*        USING OFFICED,R4                                                       
*        XC    0(OFCLENQ,R4),0(R4)                                              
*        MVI   OFCSYS,C'S'                                                      
*        MVC   OFCAUTH,TWAACCS                                                  
*        MVC   OFCLMT,TWAACCS                                                   
*        MVC   OFCAGY,AGENCY                                                    
*        CLI   1(R5),0            **IF ZERO                                     
*        BNE   *+8                                                              
*        MVI   1(R5),X'40'        **MAKE 2ND POSITION BLANK                     
*        MVC   OFCOFC2,0(R5)                                                    
*        GOTO1 AOFFICER,DMCB,(C'2',OFFICED),(0,ACOMFACS)                        
*        CLI   0(R1),0                                                          
*        BE    OFFICEOK                                                         
*        MVI   ERROR,INVALID                                                    
*        B     TRAPERR                                                          
*OFFICEOK MVC   NBSELOFF,OFCOFC      SET 1 BYTE INTERNAL OFFICE CODE            
*         DROP  R4                                                              
**       MVC   NBSELOFF,0(R5)      FILL IN OFFICE FILTER                        
**       CLI   0(R5),C'-'          CHECK FOR NEGATIVE OFFICE FILT               
**       BNE   DOOFF1D             (*-O)                                        
**       MVC   NBSELOFF,1(R5)                                                   
**       OI    NBOFFTYP,X'40'      INDICATE NEGATIVE FILTER                     
* - IF LIMITED ACCESS CHECK REQUESTED FILTER                                    
*DOOFF1D  CLI   TWAACCS,C'*'        IS THERE LIMIT ACCESS                       
*****    BNE   XIT                 NO/EXIT                                      
*        BE    DOOFF1DA               YES OK                                    
*        CLI   TWAACCS,C'$'           IF $ OK                                   
*        BE    XIT                                                              
*        CLI   TWAACCS,C'-'           IF NEGATIVE FILTERING, OK                 
*        BE    XIT                                                              
*        CLI   TWAACCS,0              OR OPEN, OK                               
*        BE    XIT                                                              
*        B     VVCLIERR            ELSE ERROR                                   
*                                                                               
DOOFF1DA CLI   TWAACCS+1,C'A'        CHECK IF ALPHA                             
         BL    DOOFF1E                                                          
         CLI   TWAACCS+1,C'Z'                                                   
         BH    DOOFF1E                                                          
         CLI   TWAACCS+2,C'0'         CHECK IF NUMERIC                          
         BL    DOOFF1E                                                          
         CLI   TWAACCS+2,C'9'                                                   
         BH    DOOFF1E                                                          
         B     XIT                    PASSED TEST SO CLIENT GROUP LIMIT         
*                                     ACCESS / IN VVCLIOUT                      
*                                                                               
DOOFF1E  CLI   TWAACCS+1,C'-'      YES/IS IT NEGATIVE OFFICE ACCESS             
         BE    DOOFF1F                                                          
* - POSITIVE OFFICE LIMIT ACCESS (*N)                                           
         TM    NBOFFTYP,X'40'          NEGATIVE REQUEST IS INVALID              
         BO    VVCLIERR                                                         
         CLC   TWAACCS+1(1),NBSELOFF   OFFICE FILTER MUST MATCH                 
         BNE   VVCLIERR                                                         
         B     XIT                                                              
* - NEGATIVE OFFICE LIMIT ACCESS (*-N)                                          
DOOFF1F  TM    NBOFFTYP,X'40'           ..IF NEGATIVE REQUEST                   
         BNO   DOOFF1G                                                          
         CLC   TWAACCS+2(1),NBSELOFF    ..NEGATIVE OFFICES MUST MATCH           
         BE    XIT                                                              
         B     VVCLIERR                                                         
DOOFF1G  CLC   TWAACCS+2(1),NBSELOFF      POSITIVE REQUEST                      
         BE    VVCLIERR                   ERROR IF MATCH                        
         B     XIT                                                              
                                                                                
         SPACE 1                                                                
DOOFF2   LA    R4,DUB              HAVE OFFICE LIST                             
         USING OFFICED,R4                                                       
         XC    0(OFCLENQ,R4),0(R4)                                              
         OI    OFCINDS,OFCIMOLC    MEDIA OFFICE LIST FOR CONVERSION             
         MVC   OFCAGY,AGENCY                                                    
*                                                                               
         OI    NBOFFTYP,X'80'      NOTE OFFICE LIST                             
         MVC   OFCMOL2,1(R5)                                                    
         CLI   0(R5),C'-'          CHECK FOR NEGATIVE OFFICE LIST               
         BNE   *+14                ($-L)                                        
         MVC   OFCMOL2,2(R5)                                                    
         OI    NBOFFTYP,X'40'      INDICATE NEGATIVE FILTER                     
*                                                                               
         CLI   OFCMOL2+1,C' '     **IF ONLY 1 CHARACTER OFFICE LIST             
         BNL   *+8                                                              
         MVI   OFCMOL2+1,C' '     **MAKE 2ND POSITION BLANK                     
*                                                                               
         GOTO1 AOFFICER,DMCB,(C'2',OFFICED),(0,ACOMFACS)                        
         BE    *+12                                                             
         MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
         MVC   NBSELOFF,OFCMOL      SET 1 BYTE INTERNAL OFFICE LIST             
         B     XIT                                                              
         DROP  R4                                                               
*                                                                               
*********  DATA SET NEMEDGEN   AT LEVEL 066 AS OF 01/25/07 ******               
*                                                                               
DOOFFX   DS    0H                 CHK FOR LIMIT ACCESS                          
         ICM   RF,15,NBSECSV                                                    
         BZ    LVCAXIT            NONE -                                        
         USING SECD,RF                                                          
         OC    SECCLAL,SECCLAL     CLIENT STRING SECURITY?                      
         BNZ   LVCAXIT             YES-LET'EM THROUGH - NETIO FILTERS           
         OC    NBTWAACC(2),NBTWAACC LIMIT ACCESS?                               
         BZ    LVCAXIT                                                          
         CLC   NBTWAACC+1(1),NBSELOFF    NO- OFFICE MUST MATCH                  
         BNE   VVCLIERR                                                         
LVCAXIT  B     XIT                                                              
         DROP  R6,RF                                                            
         DROP  R3                                                               
*DOOFFX   L     R6,ATWA            CHK FOR LIMIT ACCESS                         
*         OC    6(2,R6),6(R6)                                                   
*         BZ    LVCAXIT                                                         
*         CLC   7(1,R6),NBSELOFF                                                
*         BNE   VVCLIERR                                                        
*         B     XIT                                                             
*         SPACE                                                                 
*LVCAXIT  ICM   RF,15,NBSECSV         IF SECURITY SET                           
*         BZ    LVCAXITX                                                        
*         CLC   =C'ALL',NBSELCLI      AND CLIENT = ALL                          
*         BNE   LVCAXITX                                                        
*         USING SECD,RF                                                         
*         OC    SECCLAL,SECCLAL       AND CLIENT STRING ACCESS?                 
*        BZ    LVCAXITX                                                         
*        MVI   ERROR,SECLOCK                                                    
*        B     TRAPERR                                                          
*LVCAXITX B     XIT                                                             
*        DROP  R6,RF                                                            
*        DROP  R3                                                               
*********  DATA SET NEMEDGEN   AT LEVEL 066 AS OF 01/25/07 ******               
         EJECT                                                                  
*              POSITION CURSOR TO CORRECT FIELD IN ERRORS                       
         SPACE 3                                                                
*              INPUTS              R2=A(SCREEN HEADER)                          
*                                  FIELDERR=NUMBER OF FIELD IN ERROR            
         SPACE 1                                                                
VVCURSOR CLI   FIELDERR,0          APPLICATION MUST SET FIELD NUMBER            
         BE    TRAPERR                                                          
         L     R4,ATIOB                                                         
         USING TIOBD,R4                                                         
         OI    6(R2),X'80'         TRANSMIT ERROR FIELD HEADER                  
         OI    TIOBINDS,TIOBSETC   INSTRUCT CURSOR SETTING                      
         LR    RF,R2                                                            
         S     RF,ATWA                                                          
         STCM  RF,3,TIOBCURD       DISPLACEMENT FROM START OF TWA               
         LA    RE,8(R2)                                                         
         SR    R1,R1               COMPUTE FIELD DISPLACEMENT INTO R1           
         ZIC   R0,5(R2)            R0 HAS FIELD LENGTH                          
         ZIC   RF,FIELDERR                                                      
         BCT   RF,CURSERR2         CHECK IF ERROR IS IN FIELD 1                 
         B     CURSERR4                                                         
         SPACE 1                                                                
CURSERR2 CLI   0(RE),C','          SCAN FOR THE COMMAS                          
         BNE   CURSERR4                                                         
         BCT   RF,CURSERR4                                                      
         LA    R1,1(R1)            FOUND ENOUGH - SPACE PAST LAST               
         B     CURSERR6                                                         
         SPACE 1                                                                
CURSERR4 LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,CURSERR2                                                      
         SR    R1,R1               ERROR - DIDN'T FIND ENOUGH COMMAS            
         SPACE 1                                                                
CURSERR6 STC   R1,TIOBCURI         SET CURSOR DISPLACEMENT WITHIN FIELD         
         B     TRAPERR                                                          
         EJECT                                                                  
*              FILL TBUFF WITH TARGET DEMO NO AND NAME                          
         SPACE 3                                                                
*              LOCALS              R2=A(TARGET BUFFER)                          
*                                  R3=A(ENTRY IN CLIST)                         
*                                  R4=A(ESTIMATE RECORD)                        
         SPACE 1                                                                
VVTARBUF BRAS  RE,TARBUF                                                        
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO OUTPUT PRODUCT                                        
         SPACE 3                                                                
*                                  ASSUMES AGY/CLI IN NETBLOCK                  
         SPACE 1                                                                
VVPRDOUT DS    0H                                                               
         MVI   NBSELMOD,NBVALPRD                                                
         NETGO NSNETIO,DMCB,NETBLOCK                                            
         MVI   NBSELMOD,0          RESET SELECT MODE                            
         MVC   ERROR,NBERROR       RETURNED                                     
         CLI   NBERROR,NBGOOD                                                   
         BNE   TRAPERR                                                          
         B     XIT                                                              
         EJECT                                                                  
*              STANDARD VALIDATE PRODUCT                                        
         SPACE 3                                                                
*              ARGUMENTS           1  A(OUTPUT PRODUCT NAME)                    
*              OUTPUT              NBSLEPRD                                     
         SPACE 1                                                                
VVPRD    L     R3,0(R1)            SAVE ARG LIST                                
         MVI   NDPRDRMD,NDTOG                                                   
         ST    R2,NDPRDSCR         SAVE ADDRESS OF SCREEN HEADER                
         XC    NBSELPRD,NBSELPRD   RESET                                        
         IFMVC R3,L'PNAME,BLANKS   INITIALIZE FIRST ARG TO SPACES               
         NETGO NVGETFLD,DMCB                                                    
         BZ    XITVPRD             IF NO INPUT                                  
         XC    BLOCK(64),BLOCK                                                  
         CLC   FLD(5),=C'UNALL'                                                 
         BNE   *+12                                                             
         MVI   NBSELFLT,6                                                       
         B     LVP2                                                             
         GOTO1 SCANNER,DMCB,(R2),(2,BLOCK)     MAY HAVE ,NN (LENGTH)            
         MVC   NBSELPRD,BLOCK+12                                                
         CLC   NBSELPRD,=C'ALL'                                                 
         BNE   LVP2                                                             
         MVI   ERROR,INVPROD       DONT ALLOW ALL                               
         B     TRAPERR                                                          
         SPACE 1                                                                
LVP2     NETGO NVPRDOUT,DMCB       FILL STANDARD STUFF                          
         MVI   NDPRDRMD,NDONE                                                   
         L     R6,NBAIO            CLIENT RECORD                                
         USING PRDHDR,R6                                                        
         IFMVC R3,L'PNAME,PNAME      MOVE IF FIRST ARG GIVEN                    
         SPACE 1                                                                
         CLI   BLOCK+32,0          CHECK FOR LENGTH (,NN)                       
         BE    XITVPRD                                                          
         MVC   NBSELLEN,BLOCK+32+7                                              
         CLI   NBSELLEN,0          CHECK NUMERIC LENGTH SELECTION               
         BNE   XITVPRD                                                          
         MVI   ERROR,NOTNUM                                                     
         B     TRAPERR                                                          
         SPACE 1                                                                
XITVPRD  B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*              VALIDATE PRODUCT - SPECIAL CASE=ALL                              
         SPACE 3                                                                
*              ARGUMENTS           1  A(OUTPUT PRODUCT NAME)                    
*                                  SPACES RETURNED IF PRD=ALL                   
*              OUTPUT              NBSELPRD                                     
         SPACE 1                                                                
VVPRDALL L     R3,0(R1)            SAVE ARG LIST                                
         MVI   NDPRDRMD,NDTOG                                                   
         MVI   NBSELFLT,0                                                       
         ST    R2,NDPRDSCR         SAVE ADDRESS OF SCREEN HEADER                
         MVC   NBSELPRD,=C'POL'                                                 
         IFMVC R3,L'PNAME,BLANKS   INITIALIZE FIRST ARG TO SPACES               
         BAS   RE,NEGPGRP          ..SEE IF NEGATIVE PRDGROUP FILTERING         
         BE    VVPRDG2             ..YES                                        
         NETGO NVGETFLD,DMCB                                                    
         BZ    VVPALL2             NO INPUT=POL                                 
         CLC   8(5,R2),=C'UNALL'   CHECK FOR UNALL=UNALLOCATED                  
         BNE   VVPALL4                                                          
         MVI   NBSELFLT,6                                                       
         SPACE 1                                                                
VVPALL2  NETGO NVPRDOUT,DMCB                                                    
         B     VVPALLEX                                                         
         SPACE 1                                                                
VVPALL4  XC    BLOCK(64),BLOCK                                                  
         GOTO1 SCANNER,DMCB,(R2),(2,BLOCK)     MAY HAVE ,NN (LENGTH)            
*                                              OR AAA,BBB PIGGYBACKS            
         MVI   NDPRDRMD,NDSEP                                                   
         CLI   FLD+1,C'='          CHECK FOR GROUP                              
         BE    VVPRDGRP                                                         
         MVC   NBSELPRD,BLOCK+12                                                
         NETGO NVPRDOUT,DMCB       FILL STANDARD STUFF                          
         CLC   NBSELPRD,=C'ALL'    IF ALL GIVEN                                 
         BE    VVPALLEN              THEN EXIT                                  
         MVI   NDPRDRMD,NDONE                                                   
         L     R6,NBAIO            PRODUCT RECORD                               
         USING PRDHDR,R6                                                        
         IFMVC R3,L'PNAME,PNAME   MOVE IF FIRST ARG GIVEN                       
         B     VVPALLEN                                                         
         SPACE 1                                                                
VVPRDGRP MVC   NBSELPGR(1),FLD    GROUP PASS X0000 FOR X=POL                    
         CLC   FLD+2(3),=C'POL'                                                 
         BE    VVPRDG2                                                          
         MVC   NBSELPRD,=C'ALL'    AND FOR X=ALL                                
         CLC   FLD+2(3),=C'ALL'                                                 
         BE    VVPRDG2                                                          
         MVC   NBSELPGR+1(4),FLD+2        OR X1234 FOR X=1234                   
         SPACE 1                                                                
VVPRDG2  NETGO NVPRDOUT,DMCB                                                    
         MVI   NDPRGRMD,1                                                       
         CLI   FLD+5,C'*'                                                       
         BNE   VVPALLEX                                                         
         MVI   NDPRGRMD,2                                                       
         SPACE 1                                                                
VVPALLEN CLI   BLOCK+32,0          CHECK FOR LENGTH (,NN)                       
         BE    VVPALLEX                                                         
         MVC   NBSELLEN,BLOCK+32+7                                              
         CLI   NBSELLEN,0          CHECK NUMERIC LENGTH SELECTION               
         BNE   VVPALLEX                                                         
         SPACE 1                                                                
*                                  NOW TESTING FOR AAA,BBB PIGGYBACKS           
         MVC   NBSELPIG(1),NBEFFPNM      SET UP NBSELPIG WITH NUMBERS           
         MVC   NBSELPRD,BLOCK+32+12                                             
         CLI   NBSELPRD+2,C'*'                                                  
         BNE   *+8                                                              
         MVI   NBSELPRD+2,C' '                                                  
         MVI   NBRETURN,X'03'      REVALIDATING PRODUCT                         
         NETGO NVPRDOUT,DMCB                                                    
         MVC   NBSELPIG+1(1),NBEFFPNM                                           
*                                                                               
         MVC   NBSELPG3(3),BLOCK+12          3 CHARACTER PRODS                  
         MVC   NBSELPG3+3(3),BLOCK+32+12                                        
*                                                                               
         MVC   NBSELPRD,=C'ALL'                                                 
         MVI   NBEFFPNM,0                                                       
         CLI   BLOCK+32+12+2,C'*'  TRAILING * MEANS TREAT AS ALL                
         BE    VVPALPIG                                                         
         CLI   BLOCK+32+12+3,C'*'                                               
         BE    VVPALPIG                                                         
         MVC   NBSELPRD,=C'POL'    ELSE TREAT AS POOL                           
         SPACE 1                                                                
VVPALPIG MVI   NBRETURN,X'03'      REVALIDATING PRODUCT                         
         NETGO NVPRDOUT,DMCB       THEN RESET AS THOUGH IT WAS ALL/POL          
         IFMVC R3,L'PNAME,=CL20'PIGGYBACK COMBO.'                               
*                                                                               
         SPACE 1                                                                
VVPALLEX DS    0H                                                               
*                                                                               
VPALLXX  B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* - SEE IF NEGATINVE FILTERING/ SET NEGATIVE PGROUP FILTER IN PGRPSV            
*                               STRIP MINUS'S FROM SCREEN FIELD                 
NEGPGRP  NTR1                                                                   
***      XC    PGRPSV,PGRPSV                                                    
         LA    R1,8(R2)                                                         
         ZIC   R3,5(R2)                                                         
         CLI   5(R2),0             ...NO INPUT                                  
         BNE   *+8                                                              
         LA    R3,1(R3)            ...FUDGE TO GET NOT =                        
         CLI   0(R1),C'-'          IS THERE A MINUS                             
         BE    NPG5                                                             
         LA    R1,1(R1)                                                         
         BCT   R3,*-12                                                          
         B     NPGX                                                             
*                                                                               
NPG5     LA    R3,WORK+20           YES/MINUS/SET PGGROUP                       
*NPG5     LA    R3,PGRPSV           YES/MINUS/SET PGGROUP                       
         XC    WORK,WORK                                                        
         ZIC   R4,5(R2)                                                         
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),8(R2)       SET INPUT FIELD INTO WORK                    
         LA    R2,WORK                                                          
NPG10    CLI   0(R2),C'='                                                       
         BE    NPG22                                                            
         CLI   0(R2),C'-'          IF MINUS                                     
         BNE   NPG20                                                            
         LA    R2,1(R2)            SKIP MINUS                                   
         MVC   0(1,R3),0(R2)       SET INTO PGRPSV                              
         NI    0(R3),X'FF'-X'40'   TURN OFF X'40'                               
         B     *+10                                                             
NPG20    MVC   0(1,R3),0(R2)                                                    
         LA    R3,1(R3)            BUMP PGRPSV                                  
NPG22    LA    R2,1(R2)            BUMP WORK                                    
         CLI   0(R2),X'40'                                                      
         BH    NPG10                                                            
NPG30    DS    0H                                                               
***      MVC   NBSELPGR(1),PGRPSV                                               
***      CLC   =C'POL',PGRPSV+1                                                 
         MVC   NBSELPGR(1),WORK+20                                              
         CLC   =C'POL',WORK+21                                                  
         BE    NPG32                                                            
         MVC   NBSELPRD,=C'ALL'                                                 
***      CLC   =C'ALL',PGRPSV+1                                                 
         CLC   =C'ALL',WORK+21                                                  
         BE    NPG32                                                            
***      MVC   NBSELPGR+1(4),PGRPSV+1                                           
         MVC   NBSELPGR+1(4),WORK+21                                            
NPG32    XC    FLD,FLD                                                          
         B     NPGX                                                             
*                                                                               
NPGX     B     XIT                                                              
*                                                                               
***PGRPSV   DS    CL5                                                           
PSCRNSV  DS    CL15                                                             
         EJECT                                                                  
*              ROUTINE TO OUTPUT ESTIMATE                                       
         SPACE 3                                                                
*                                  ASSUMES AGY/CLI/PRD IN NETBLOCK              
         SPACE 1                                                                
VVESTOUT MVI   NBSELMOD,NBVALEST                                                
         NETGO NSNETIO,DMCB,NETBLOCK                                            
         MVI   NBSELMOD,0          RESET SELECT MODE                            
         MVC   ERROR,NBERROR       RETURNED                                     
         CLI   NBERROR,NBGOOD                                                   
         BNE   TRAPERR                                                          
         CLI   NDESTRMD,NDONE      IF ONE ESTIMATE WAS SELECTED                 
         BNE   XIT                                                              
         MVI   NBDONTFD,C'Y'       TELL NETIO NOT TO FILTER DATES               
         B     XIT                                                              
         EJECT                                                                  
*              STANDARD VALIDATE ESTIMATE                                       
         SPACE 3                                                                
*              ARGUMENTS           1  A(OUTPUT ESTIMATE NAME)                   
*                                  2  A(NET DEMO BLOCK)                         
*              OUTPUT              NBSELEST + THOSE IN PROCEST                  
*                                  IF ARG 2 THEN NDDEMOS NDNDEMOS               
         SPACE 1                                                                
VVEST    L     R3,0(R1)            SAVE ARG LIST                                
         MVI   NDESTRMD,NDTOG                                                   
         ST    R2,NDESTSCR         SAVE ADDRESS OF SCREEN HEADER                
         L     R4,4(R1)                                                         
         IFMVC R3,24,BLANKS        INITIALIZE FIRST ARG TO SPACES               
         NETGO NVGETFLD,DMCB                                                    
         BZ    VVE2                IF NO INPUT                                  
         MVI   NDESTRMD,NDONE                                                   
         LTR   R0,R0               CHECK NUMERIC                                
         BNZ   VVE2                                                             
         MVI   ERROR,NOTNUM                                                     
         B     TRAPERR                                                          
         SPACE 1                                                                
VVE2     STC   R0,NBSELEST                                                      
         NETGO NVESTOUT,DMCB       FILL STANDARD STUFF                          
         CLI   NBSELEST,0          DONT PROCEST IF NONE GIVEN                   
         BE    VVEXIT                                                           
         BAS   RE,PROCEST                                                       
         SPACE 1                                                                
VVEXIT   B     XIT                                                              
         EJECT                                                                  
*              VALIDATE ESTIMATE - SPECIAL CASE=ALL                             
         SPACE 3                                                                
*              ARGUMENTS           1  A(OUTPUT ESTIMATE NAME)                   
*                                  SPACES RETURNED IF EST=ALL                   
*                                  2  A(NET DEMO BLOCK)                         
*              OUTPUT              NBSELEST + THOSE IN PROCEST                  
*                                  IF ARG 2 THEN NDDEMOS NDNDEMOS               
         SPACE 1                                                                
VVESTALL L     R3,0(R1)            ARG LIST                                     
         L     R4,4(R1)                                                         
         MVI   NDESTRMD,NDTOG                                                   
         ST    R2,NDESTSCR         SAVE ADDRESS OF SCREEN HEADER                
         IFMVC R3,24,BLANKS        INITIALIZE FIRST ARG TO SPACES               
         NETGO NVGETFLD,DMCB                                                    
         BZ    VEA2                NO INPUT = ALL                               
         MVI   NDESTRMD,NDSEP                                                   
         CLC   FLD(3),=C'ALL'         IF ALL, SELEST IS 0 SO GO ON.             
         BE    VEA2                                                             
         MVI   NDESTRMD,NDONE                                                   
         LTR   R0,R0               MAKE SURE ITS NUMERIC                        
         BNZ   VEA2                                                             
         MVI   ERROR,NOTNUM                                                     
         B     TRAPERR                                                          
         SPACE 1                                                                
VEA2     STC   R0,NBSELEST         NUMERIC VALUE OF INPUT                       
         NETGO NVESTOUT,DMCB       FILL STANDARD STUFF                          
         CLI   NBSELEST,0          DONT PROCEST IF NONE GIVEN                   
         BE    VEAXIT                                                           
         BAS   RE,PROCEST          FILL FIELDS                                  
         SPACE 1                                                                
VEAXIT   B     XIT                                                              
         EJECT                                                                  
*              VALIDATE ESTIMATE RANGE                                          
         SPACE 3                                                                
*              SPECIAL CASE        ALL, NO, RANGE OF ESTS                       
*                                                                               
*              ARGUMENTS           1  A(OUTPUT ESTIMATE NAME)                   
*                                  SPACES RETURNED IF EST=ALL                   
*                                  2  A(NET DEMO BLOCK)                         
*              OUTPUT              NBSELEST + THOSE IN PROCEST                  
*                                  IF ARG 2 THEN NDDEMOS NDNDEMOS               
*              LOCAL               R5 FOR ESTIMATE FILTERS                      
         SPACE 1                                                                
VVESTRNG LM    R3,R4,0(R1)         ARG LIST                                     
         MVI   NDESTRMD,NDTOG                                                   
         ST    R2,NDESTSCR         SAVE ADDRESS OF SCREEN HEADER                
         IFMVC R3,24,BLANKS        INITIALIZE FIRST ARG TO SPACES               
         XC    BLOCK(96),BLOCK                                                  
         GOTO1 SCANNER,DMCB,(R2),(3,BLOCK)   LOOK FOR RANGE                     
         MVI   NBSELEST,0          PRESET TO NO RANGE                           
         MVI   NBSELESE,0                                                       
         CLI   DMCB+4,2            MUST BE 2 PARTS FOR RANGE                    
         BH    ESTBADRG                                                         
         BL    VER1                                                             
         CLC   BLOCK+12(2),=C'NO'  CK FOR NO,ESTFILT                            
         BE    VER1                                                             
         CLC   BLOCK+12(3),=C'ALL' OR FOR ALL,ESTFILT                           
         BE    VER1                                                             
         MVC   NBSELEST,BLOCK+7      FIRST RANGE VALUE                          
         CLI   NBSELEST,0          ZERO MEANS NON NUMERIC OR ZERO               
         BE    ESTBADRG                                                         
         MVC   NBSELESE,BLOCK+39   SECOND RANGE VALUE                           
         CLC   NBSELESE,NBSELEST   CK FOR 0 OR LESS THAN 1ST VALUE              
         BNH   ESTBADRG                                                         
         NETGO NVESTOUT,DMCB                                                    
         B     VERXIT                                                           
         SPACE 1                                                                
VER1     NETGO NVGETFLD,DMCB       ONLY ONE OR NONE GIVEN OR NO,ESTFILT         
         BZ    VER4                NO INPUT = NO                                
         LA    R5,BLOCK+44         R5=A(POSSIBLE FILTER EXPRESSION)             
         CLC   FLD(2),=C'NO'                                                    
         BE    VER2                                                             
         LA    R5,BLOCK+22                                                      
         CLC   FLD(2),=C'F='                                                    
         BE    VER2                                                             
         MVI   NDESTRMD,NDSEP                                                   
         LA    R5,BLOCK+44                                                      
         CLC   FLD(3),=C'ALL'         IF ALL, SELEST IS 0 SO GO ON.             
         BE    VER2                                                             
         MVI   NDESTRMD,NDONE                                                   
         LTR   R0,R0               MAKE SURE ITS NUMERIC                        
         BZ    VER1A                                                            
         STC   R0,NBSELEST         NUMERIC VALUE OF INPUT                       
         B     VER4                                                             
         SPACE 1                                                                
VER1A    MVI   ERROR,NOTNUM                                                     
         B     TRAPERR                                                          
         SPACE 1                                                                
VER2     CLI   0(R5),X'41'         IF ANYTHING IS IN BLOCK+NN                   
         BL    VER4                   IT MUST BE FILTER EXPRESSION              
         NETGO NVEFOUT,DMCB           SO DEAL WITH FILTERS NOW                  
         SPACE 1                                                                
VER4     NETGO NVESTOUT,DMCB       FILL STANDARD STUFF                          
         CLI   NBSELEST,0          DONT PROCEST IF NONE GIVEN                   
         BE    VERXIT                                                           
         BAS   RE,PROCEST          FILL FIELDS                                  
         SPACE 1                                                                
VERXIT   B     XIT                                                              
         SPACE 1                                                                
ESTBADRG MVI   ERROR,NBINVEST                                                   
         B     TRAPERR                                                          
         EJECT                                                                  
*              FILL FIELDS FOR ESTIMATE RECORD                                  
         SPACE 3                                                                
*                                  INTERNAL - NO FROM NETGO                     
*              INPUTS              NBAIO=A(ESTIMATE RECORD)                     
*                                  R3=A(ESTNAME)                                
*                                  R4=A(DEMO BLOCK)                             
*              OUTPUTS             USERQSTR USERQEND NBSELSTR NBSELEND          
*                                  NDWGTLST NDDEMOS NDUSRNMS                    
         SPACE 1                                                                
PROCEST  NTR1                                                                   
         BRAS  RE,XPROCST                                                       
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE ESTIMATE FILTER                              
         SPACE 3                                                                
*              OUTPUT              NBSELEFL                                     
         SPACE 1                                                                
VVEFILT  DS    0H                                                               
         NETGO NVGETFLD,DMCB                                                    
         BZ    VVEFLXIT                                                         
         LA    R5,FLD              SET ARG                                      
         NETGO NVEFOUT,DMCB                                                     
         SPACE 1                                                                
VVEFLXIT B     XIT                                                              
         EJECT                                                                  
*              ESTIMATE FILTER OUT                                              
         SPACE 3                                                                
*              INPUT               R5=A(BEGINNING OF FIELD)                     
*                                  MUST BE SPACE TERMINATED                     
         SPACE 1                                                                
VVEFOUT  DS    0H                                                               
         XC    NBSELEFL,NBSELEFL   RESET FILTER FIELD                           
         LA    R4,NBSELEFL                                                      
         LA    R3,L'NBSELEFL                                                    
         SPACE 1                                                                
VVEFLOOP CLI   0(R5),X'40'         END OF LIST                                  
         BE    VVEFXIT                                                          
         CLI   0(R5),C'*'          WILD CARD. ACCEPTABLE                        
         BE    VVEFNXT                                                          
         CLI   0(R5),C'-'          MINUS FILT.                                  
         BNE   VVEF2                                                            
         LA    R5,1(R5)                                                         
         NI    0(R5),X'FF'-X'40'   TURN OFF X'40' BIT                           
         B     VVEFNXT                                                          
         SPACE 1                                                                
VVEF2    CLI   0(R5),X'C0'         MUST BE A LETTER OR NUMBER                   
         BL    VVEFERR                                                          
         SPACE 1                                                                
VVEFNXT  MVC   0(1,R4),0(R5)                                                    
         LA    R4,1(R4)                                                         
         LA    R5,1(R5)                                                         
         BCT   R3,VVEFLOOP                                                      
         CLI   0(R5),X'40'         MUST BE LAST ONE                             
         BNE   VVEFERR                                                          
         SPACE 1                                                                
VVEFXIT  B     XIT                                                              
         SPACE 1                                                                
VVEFERR  MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
         EJECT                                                                  
*              VALIDATE NETWORK                                                 
         SPACE 3                                                                
*              SPECIAL CASE        M=MEDIA CODE                                 
*                                  OR ALL=MEDIA CODE                            
*              ARGUMENT            1  A(COMPRESSED MKT NUMBER)                  
*              OUTPUT              NBSELNET OR NBSELMFL                         
         SPACE 1                                                                
VVNETALL L     R3,0(R1)            SAVE ARG 1                                   
         MVI   NDNETRMD,NDTOG                                                   
         ST    R2,NDNETSCR         SAVE ADDRESS OF SCREEN HEADER                
         XC    NBSELNET,NBSELNET   RESET NBSELNET                               
         MVI   NBUSRMED,C'N'                                                    
         NETGO NVGETFLD,DMCB                                                    
         BZ    VNAXIT              IF NO INPUT                                  
*                                                                               
         CLI   FLD+1,C'='          STATION GROUPS?                              
         BNE   VVNALL0                                                          
         BRAS  RE,STAGRP                                                        
         BNE   TRAPERR                                                          
         B     VNAXIT                                                           
VVNALL0  CLC   FLD(3),=C'ALL'      ALL IS OK                                    
         BNE   VVNALL4             NO VALIDATION REQUIRED FOR ALL               
         MVI   NDNETRMD,NDSEP                                                   
         CLI   FLD+3,C'='          CHECK FOR ALL=MEDIA                          
         BE    VVNALL1                                                          
         CLI   FLD+3,C','                 OR ALL,MEDIA                          
         BE    VVNALL1                                                          
         CLI   FLD+3,X'40'                 ANYTHING ELSE IS ERROR               
         BNH   VNAXIT                                                           
         MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
         SPACE 1                                                                
VVNALL1  CLI   FLD+4,C'-'           CHECK NEGATIVE FILTERING                    
         BNE   VVNALL3                                                          
         MVC   NBSELMFL,FLD+5                                                   
         NI    NBSELMFL,X'FF'-X'40'    TURN OFF X'40' BIT                       
         B     VNAXIT                                                           
VVNALL3  MVC   NBSELMFL,FLD+4                                                   
         MVC   NBUSRMED,NBSELMFL                                                
         B     VNAXIT                                                           
         SPACE 1                                                                
VVNALL4  CLC   FLD(2),=C'M='       CHECK FOR M=MEDIA CODE                       
         BE    VVNALL6                                                          
         CLI   FLD,C'-'           CHECK NEGATIVE FILTERING                      
         BE    VVNALL8                                                          
         CLC   FLD(2),=C'NO'       CHECK NO=MEDIA CODE                          
         BNE   VVNETCOM                                                         
         CLI   FLD+2,X'40'                                                      
         BE    *+12                                                             
         CLI   FLD+2,C','                                                       
         BNE   VVNETCOM                                                         
         MVC   NBSELMFL,FLD+3                                                   
         MVC   NBUSRMED,NBSELMFL                                                
         B     VNAXIT                                                           
VVNALL6  MVC   NBSELMFL,FLD+2                                                   
         MVC   NBUSRMED,NBSELMFL                                                
         B     VNAXIT                                                           
VVNALL8  MVC   NBSELMFL,FLD+1                                                   
         NI    NBSELMFL,X'FF'-X'40'    TURN OFF X'40' BIT                       
         B     VNAXIT                                                           
         SPACE 1                                                                
*                                  SAME WITHOUT ALL                             
VVNET    L     R3,0(R1)            SAVE ARG 1                                   
         MVI   NDNETRMD,NDTOG                                                   
         ST    R2,NDNETSCR         SAVE ADDRESS OF SCREEN HEADER                
         XC    NBSELNET,NBSELNET   RESET NBSELNET                               
         NETGO NVGETFLD,DMCB                                                    
         BZ    VNAXIT              IF NO INPUT                                  
         SPACE 1                                                                
VVNETCOM MVC   NBSELNET,FLD        NETWORK GIVEN                                
         MVI   NDNETRMD,NDONE                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING STAREC,R4                                                        
         MVI   STAKTYPE,C'S'                                                    
         MVI   STAKMED,C'N'                                                     
         MVC   STAKCALL(4),NBSELNET                                             
         MVI   STAKCALL+4,C'N'                                                  
         MVC   STAKAGY,NBSELAGY                                                 
         MVC   STAKCLT,=CL3'000'   FILL WITH CHAR ZEROS                         
         MVC   STAKFILL,=CL5'00000'                                             
         MVC   FILENAME,=C'STATION '                                            
         MVI   USEIO,C'Y'          PUT STATION RECORD IN I/O AREA               
         GOTO1 READ                                                             
         BE    VNAOK               VALID NETWORK                                
         MVI   USEIO,C'N'          RESET                                        
         XC    FILENAME,FILENAME                                                
         MVI   ERROR,INVSTAT                                                    
         B     TRAPERR                                                          
         SPACE 1                                                                
VNAOK    MVI   USEIO,C'N'          RESET                                        
         L     R4,NBAIO                                                         
         MVC   NBSTATYP,STYPE      PICK UP MEDIA FOR PROFILES                   
         XC    FILENAME,FILENAME                                                
         LTR   R3,R3               FILL OPTIONAL ARG                            
         BZ    VNAXIT                                                           
         LA    R4,IO                                                            
         PACK  DUB,SMKT                                                         
         CVB   R1,DUB                                                           
         STH   R1,0(R3)                                                         
         SPACE 1                                                                
VNAXIT   CLI   NBUSRMED,X'41'      IF SPECIFIC MEDIA IS SELECTED                
         B     XIT                                                              
****     BL    XIT                                                              
         CLI   NBUSRMED,C'N'       AND IT IS NOT NETWORK,                       
         BE    XIT                                                              
         XC    KEY,KEY             CHECK FOR SPECIFIC PROFILES                  
         MVC   KEY(4),=C'S0N0'                                                  
         MVC   KEY+4(2),NBEFFAGY                                                
         MVC   KEY+6(1),NBUSRMED                                                
         MVC   KEY+7(3),NBCLICOD                                                
         GOTO1 NBGTPROF,DMCB,KEY,(C'M',NBUSER),NBDM                             
         MVI   KEY+3,C'1'                                                       
         GOTO1 NBGTPROF,DMCB,KEY,(C'M',NBUSER1),NBDM                            
         MVI   KEY+3,C'2'                                                       
         GOTO1 NBGTPROF,DMCB,KEY,(C'M',NBUSER2),NBDM                            
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              VALIDATE A DAYPART                                               
         SPACE 3                                                                
*              ARGUMENTS           HIGH ORDER BYTE = 0, VALIDATE DPT            
*                                                                               
*                                  HIGH ORDER BYTE = 1, GET DPT CODE            
*                                                                               
*              OUTPUT              NBSELDPT                                     
*                                  RETURN 2 CHAR CODE + DESCRIPTION             
         SPACE 1                                                                
VVDPT    DS    0H                                                               
         ZIC   R3,0(R1)            ARG                                          
         CHI   R3,1                GET CODE/DESCRIPTION?                        
         BNE   VVDP01                                                           
         L     R5,0(R1)            ADDR DPT 1 BYTE CODE                         
         L     R3,4(R1)            OUTPUT ADDRESS                               
         B     HNDPT200            GO GET IT                                    
                                                                                
********************************  VALIDATE DAYPART CODE                         
VVDP01   L     R3,0(R1)                                                         
         MVI   NDDPTRMD,NDTOG                                                   
         ST    R2,NDDPTSCR         SAVE ADDRESS OF SCREEN HEADER                
         IFMVC R3,8,BLANKS         FILL EXPANDED DAYPART W/ BLANKS              
         NETGO NVGETFLD,DMCB                                                    
         BZ    VVDPXIT             CK FOR NO INPUT                              
         MVI   NDDPTRMD,NDONE                                                   
VVDP1    BAS   RE,NDPTVAL          NEW DAYPART VAL                              
         LTR   R3,R3               IF OPTIONAL ARG GIVEN                        
         BZ    VVDP2                                                            
         MVC   0(8,R3),WORK+2      THEN MOVE VALUE OF LOOKUP                    
         SPACE 1                                                                
VVDP2    CLI   WORK,C' '           IF NOT FOUND                                 
         BE    VVDPERR                                                          
         MVC   NBSELDP,WORK+20     ONE BYTE EQUATE                              
         MVC   NBSELNDP,FLD        2 CHAR DAYPART CODE                          
         OC    NBSELNDP,BLANKS                                                  
         SPACE 1                                                                
VVDPXIT  B     XIT                                                              
         SPACE 1                                                                
VVDPERR  MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
         EJECT                                                                  
*              VALIDATE A DAYPART (ALL ALLOWED)                                 
         SPACE 3                                                                
*              ARGUMENTS           1  A(OUTPUT EXPANDED DAYPART)                
*              OUTPUT              NBSELDPT                                     
         SPACE 1                                                                
VVDPTALL L     R3,0(R1)            SAVE ARG                                     
         MVI   NDDPTRMD,NDTOG                                                   
         ST    R2,NDDPTSCR         SAVE ADDRESS OF SCREEN HEADER                
         IFMVC R3,8,BLANKS         FILL EXPANDED DAYPART W/ BLANKS              
         NETGO NVGETFLD,DMCB                                                    
         BZ    VVDPAXIT            CK FOR NO INPUT                              
         CLC   FLD(3),=C'TOG'      ALLOW TOGETHER                               
         BE    VVDPAXIT                                                         
         CLC   FLD(2),=C'NO'                                                    
         BE    VVDPAXIT                                                         
         MVI   NDDPTRMD,NDSEP                                                   
         CLC   FLD(3),=C'ALL'                                                   
         BE    VVDPAXIT                                                         
         MVI   NDDPTRMD,NDONE                                                   
         B     VVDP1                                                            
***      BAS   RE,NDPTVAL          NEW DAYPART VALIDATION                       
***      LTR   R3,R3               IF OPTIONAL ARG GIVEN                        
***      BZ    VVDPA2                                                           
***      MVC   0(8,R3),WORK+2      THEN MOVE VALUE OF LOOKUP                    
         SPACE 1                                                                
***VVDPA2   CLI   WORK,C' '           IF NOT FOUND                              
***         BE    VVDPAERR                                                      
***         MVC   NBSELDP,WORK+20                                               
         SPACE 1                                                                
VVDPAXIT B     XIT                                                              
         SPACE 1                                                                
VVDPAERR MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
NDPTVAL  NTR1                                                                   
*  - DOES ALL USER DAYPART HANDLING                                             
*                                                                               
* AT ENTRY P1 CONTAINS BYTE 1 = 1(DAYPART RETURN)                               
*                      BYTE 1 = 0(DAYPART VALIDATION)                           
* AT ENTRY FLD CONTAINS A(2 BYTE DAYPART CODE(FOR VALIDATION))                  
*                       A(1 BYTE DAYPART CODE(FOR RETURN))                      
*                                                                               
* ON RETURN                                                                     
* WORK   = 1 OR 2 BYTE DAYPART CODE                                             
* WORK+2 = 14 BYTE DAYPART DESCRIPTION                                          
*                                                                               
         LR    R2,R1                DATA LENGTH IN FLD                          
         BCTR  R2,0                FOR EX COMPARE                               
         USING NDPTHDR,R4                                                       
         MVI   WORK,X'40'                                                       
*                                                                               
*  VALIDATE A DAYPART                                                           
*                                                                               
         NETGO NVSETUNT,DMCB                                                    
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   NDPTKTYP,X'0D'                                                   
         MVI   NDPTKTYP+1,X'07'                                                 
*!!!!    MVC   NDPTKTYP,=XL2'0D07'                                              
         MVC   NDPTAGM,NBACTAM                                                  
         MVC   FILENAME,=C'UNTDIR  '                                            
*                                                                               
HNDPT010 GOTO1 HIGH                                                             
         B     HNDPT040                                                         
*                                                                               
HNDPT020 GOTO1 SEQ                                                              
                                                                                
HNDPT040 CLC   KEY(5),KEYSAVE      AGY LEVEL/CLIENT LEVEL                       
         BNE   HNDPT100            NO                                           
         CLC   NDPTDPTA,FLD        MATCH ON CODE?                               
         BNE   HNDPT020                                                         
         MVC   WORK(2),NDPTDPTA    PASS BACK DAYPART                            
         MVC   WORK+2(14),NDPTDES  PASS BACK DESCRIPTION                        
         MVC   WORK+20(1),NDPTDPTE   PASS BACK EQUATE                           
         B     HNDPTEX                                                          
*                                                                               
* CHECK KEY FOR AGENCY OR CLIENT LEVEL CHECK                                    
* IF AGENCY LEVEL-RESET KEY-MOVE CLIENT CODE IN-RESTART SEARCH                  
* IF CLIENT LEVEL-EXIT ROUTINE-DPT WAS INVALID                                  
*                                                                               
HNDPT100 OC    KEYSAVE+3(2),KEYSAVE+3                                           
         BNZ   HNDPTBEX                                                         
*                                                                               
         OC    NBACTCLI,NBACTCLI   MUST HAVE CLI TO CONT W/ DPT CHECK           
         BZ    HNDPTBEX                                                         
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(5),KEYSAVE                                                   
         MVC   KEY+3(2),NBACTCLI                                                
         B     HNDPT010                                                         
*                                                                               
* FOLLOWING ROUTINE FINDS THE 2 CHARACTER CODE AND EXPANSION                    
* EXPECTS R5 -> 1 BYTE DPT NUMBER                                               
* EXPECTS R3 -> 16 BYTE OUTPUT AREA (2 CODE/14 DESCRIPTION)                     
* THIS ROUTINE NOT ENTERED WITH NTR1                                            
HNDPT200 DS    0H                                                               
         NETGO NVSETUNT,DMCB                                                    
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   NDPTKTYP,X'0D'                                                   
         MVI   NDPTKTYP+1,X'07'                                                 
*!!!     MVC   NDPTKTYP,=XL2'0D07'                                              
         MVC   NDPTAGM,NBACTAM                                                  
         CLI   0(R5),127            CHECK CLIENT LEVEL DAYPART                  
         BH    *+10                                                             
         MVC   NDPTCLT,NBACTCLI                                                 
         MVC   NDPTDPTE,0(R5)                                                   
         MVC   FILENAME,=C'UNTDIR'                                              
         GOTO1 HIGH                                                             
         CLC   KEY(6),KEYSAVE                                                   
         BNE   HNDPTBEX                                                         
         MVC   0(2,R3),NDPTDPTA    PASS BACK DAYPART                            
         MVC   2(14,R3),NDPTDES  PASS BACK DESCRIPTION                          
*                                                                               
HNDPTEX  MVC   KEYSAVE,KEY          ALL NEEDED DISP. FIELDS IN KEYSAVE          
HNDPTBEX XC    FILENAME,FILENAME                                                
         B     XIT                                                              
         DROP  R4                                                               
         SPACE 1                                                                
         EJECT                                                                  
*              VALIDATE A PACKAGE                                               
         SPACE 3                                                                
*                                  ASSUMES A/C/P/E IN NETBLOCK                  
         SPACE 1                                                                
VVPAKOUT MVI   NBSELMOD,NBPROCPK                                                
         MVC   BYTE,NBDATA         SAVE OFF NBDATA                              
         MVI   NBDATA,C'P'         GET PACKAGE RECORD                           
         NETGO NSNETIO,DMCB,NETBLOCK                                            
         MVC   NBDATA,BYTE         RESTORE NBDATA                               
         MVI   NBSELMOD,0          RESET SELECT MODE                            
         MVC   ERROR,NBERROR       RETURNED                                     
         CLI   NBERROR,NBGOOD                                                   
         BNE   TRAPERR                                                          
         B     XIT                                                              
         EJECT                                                                  
*              STANDARD VALIDATE PACKAGE                                        
         SPACE 3                                                                
*              ARGUMENTS           1  A(OUTPUT PACKAGE NAME)                    
*              SPECIAL CASE        PRT                                          
*              OUTPUT              NBSELPAK                                     
         SPACE 1                                                                
VVPAK    L     R3,0(R1)            SAVE ARG LIST                                
         MVI   NDPAKRMD,NDTOG                                                   
         ST    R2,NDPAKSCR         SAVE ADDRESS OF SCREEN HEADER                
         IFMVC R3,36,BLANKS        INITIALIZE FIRST ARG TO SPACES               
         NETGO NVGETFLD,DMCB                                                    
         BZ    VPAK2               IF NO INPUT                                  
         MVI   NDPAKRMD,NDONE                                                   
         CLC   FLD(3),=C'PRT'      IF PRT OPTION                                
         BNE   VPAK1                                                            
         MVI   NDPAKRMD,NDTOG                                                   
         MVI   NBSELPRN,C'Y'       SET SELPRN                                   
         MVI   NBSELPAK,0          ALLOW ALL PACKAGES                           
         CLI   FTERMFLG,1          ONLY ALLOW PRT IF FIELD IS OPTIONAL          
         BE    VPAK4                                                            
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
         SPACE 1                                                                
VPAK1    LTR   R0,R0               INSURE ITS NUMERIC                           
         BNZ   VPAK2                                                            
         MVI   ERROR,NOTNUM                                                     
         B     TRAPERR                                                          
         SPACE 1                                                                
VPAK2    STC   R0,NBSELPAK         PACKED NUMBER                                
         SPACE 1                                                                
VPAK4    NETGO NVPAKOUT,DMCB       FILL STANDARD STUFF                          
         CLI   NBSELPAK,0          DONT PRINT ANYTHING IF ALL                   
         BE    VPAKXIT                                                          
         LTR   R3,R3               MOVE PAKLINE IF 1ST ARG GIVEN                
         BZ    VPAKXIT                                                          
         MVC   0(16,R3),NBPAKNAM        NAME -DAYPART - COST                    
         MVC   17(8,R3),NBDPNAM                                                 
         MVI   26(R3),C'$'                                                      
         EDIT  (4,NBPAKCST),(8,27(R3)),ALIGN=LEFT,ZERO=NOBLANK                  
         GOTO1 SQUASHER,DMCB,0(R3),36                                           
         SPACE 1                                                                
VPAKXIT  B     XIT                                                              
         EJECT                                                                  
*              VALIDATE PACKAGE WITH LOCK OPTION                                
         SPACE 3                                                                
*              OPTIONAL ARGUMENTS  PACKAGE NAME                                 
*              SPECIAL CASES       LOCK - SELECT LOCKED PACKAGES                
*                                  BOTH - LOCKED AND UNLOCKED PACKAGES          
*                                  PRT  - DONT SHOW UNITS NO PRINT STAT         
*                                  F=XXXXXX FILTER EXPRESSION                   
*              OUTPUTS             NBSELPAK                                     
*                                  NBSELPFL (IF FILTERS)                        
         SPACE 1                                                                
VVPAKLOK L     R3,0(R1)            SAVE ARG LIST                                
         MVI   NDPAKRMD,NDTOG                                                   
         ST    R2,NDPAKSCR         SAVE ADDRESS OF SCREEN HEADER                
         IFMVC R3,36,BLANKS        INITIALIZE FIRST ARG TO SPACES               
         NETGO NVGETFLD,DMCB                                                    
         BZ    VPAKL2              IF NO INPUT                                  
         CLC   FLD(4),=C'LOCK'     LOCK OPTION                                  
         BNE   CKBOTH                                                           
         MVI   NBSELPST,C'L'                                                    
         NETGO NVPAKOUT,DMCB                                                    
         B     VPAKLXIT                                                         
         SPACE 1                                                                
CKBOTH   CLC   FLD(4),=C'BOTH'     BOTH OPTION                                  
         BNE   CKPPRT                                                           
         MVI   NBSELPST,C'B'                                                    
         NETGO NVPAKOUT,DMCB                                                    
         B     VPAKLXIT                                                         
         SPACE 1                                                                
CKPPRT   CLC   FLD(3),=C'PRT'      PRT OPTION                                   
         BNE   VPAKF2                                                           
         MVI   NBSELPRN,C'Y'                                                    
         NETGO NVPAKOUT,DMCB                                                    
         B     VPAKLXIT                                                         
         SPACE 1                                                                
VPAKF2   CLC   FLD(2),=C'F='       PACKAGE FILTERING                            
         BNE   VPAKA2                                                           
         LA    R5,FLD+2                                                         
         NETGO NVPFOUT,DMCB                                                     
         NETGO NVPAKOUT,DMCB                                                    
         B     VPAKLXIT                                                         
         SPACE 1                                                                
VPAKA2   CLC   FLD(3),=C'ALL'                                                   
         BNE   VPAKL1                                                           
         MVI   NDPAKRMD,NDSEP                                                   
         NETGO NVPAKOUT,DMCB                                                    
         B     VPAKLXIT                                                         
         SPACE 1                                                                
VPAKL1   MVI   NDPAKRMD,NDSEP                                                   
         LTR   R0,R0               INSURE ITS NUMERIC                           
         BNZ   VPAKL2                                                           
         MVI   ERROR,NOTNUM                                                     
         B     TRAPERR                                                          
         SPACE 1                                                                
VPAKL2   STC   R0,NBSELPAK         PACKED NUMBER                                
         NETGO NVPAKOUT,DMCB       FILL STANDARD STUFF                          
         CLI   NBSELPAK,0          DONT PRINT IF ALL                            
         BE    VPAKLXIT                                                         
         LTR   R3,R3               MOVE PAKLINE IF 1ST ARG GIVEN                
         BZ    VPAKLXIT                                                         
         MVC   0(16,R3),NBPAKNAM        NAME -DAYPART - COST                    
         MVC   17(8,R3),NBDPNAM                                                 
         MVI   26(R3),C'$'                                                      
         EDIT  (4,NBPAKCST),(8,27(R3)),ALIGN=LEFT,ZERO=NOBLANK                  
         GOTO1 SQUASHER,DMCB,0(R3),36                                           
         SPACE 1                                                                
VPAKLXIT B     XIT                                                              
         EJECT                                                                  
*              VALIDATE PACKAGE FILTERS                                         
         SPACE 3                                                                
*                                  OUTPUT NBSELPFL                              
VVPFILT  DS    0H                                                               
         NETGO NVGETFLD,DMCB                                                    
         BZ    VVPFLXIT                                                         
         LA    R5,FLD              SET ARG                                      
         NETGO NVEFOUT,DMCB                                                     
         SPACE 1                                                                
VVPFLXIT B     XIT                                                              
         EJECT                                                                  
*              PACKAGE FILTER HANDLING                                          
         SPACE 3                                                                
*                                  R5=A(FIELD) - SPACE TERMINATED               
         SPACE 1                                                                
VVPFOUT  DS    0H                                                               
         XC    NBSELPKF,NBSELPKF   RESET FILTER FIELD                           
         LA    R4,NBSELPKF                                                      
         LA    R3,L'NBSELPKF                                                    
         SPACE 1                                                                
VVPFLOOP CLI   0(R5),X'40'         END OF LIST                                  
         BE    VVPFXIT                                                          
         CLI   0(R5),C'*'          WILD CARD. ACCEPTABLE                        
         BE    VVPFNXT                                                          
         CLI   0(R5),C'-'          MINUS FILT.                                  
         BNE   VVPF2                                                            
         LA    R5,1(R5)                                                         
         NI    0(R5),X'FF'-X'40'   TURN OFF X'40' BIT                           
         B     VVPFNXT                                                          
         SPACE 1                                                                
VVPF2    CLI   0(R5),X'C0'         MUST BE A LETTER OR NUMBER                   
         BL    VVPFERR                                                          
         SPACE 1                                                                
VVPFNXT  MVC   0(1,R4),0(R5)                                                    
         LA    R4,1(R4)                                                         
         LA    R5,1(R5)                                                         
         BCT   R3,VVPFLOOP                                                      
         CLI   0(R5),X'40'         MUST BE LAST ONE                             
         BNE   VVPFERR                                                          
         SPACE 1                                                                
VVPFXIT  B     XIT                                                              
         SPACE 1                                                                
VVPFERR  MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
         EJECT                                                                  
*              VALIDATE A DEMO STRING                                           
         SPACE 3                                                                
*              ARGUMENTS           1  A(AREA FOR DBLOCK)                        
*                                  2  A(NET DEMO BLOCK)                         
*              INPUT               NDNDEMOS = MAX N'DEMOS                       
*              OUTPUT              NDDEMOS  LIST OF DEMOS                       
*                                  NDWGTLST RESET TO ZEROS                      
*                                  NDNDEMOS N'VALID DEMOS                       
         SPACE 1                                                                
VVDEM    BRAS  RE,XVVDEM                                                        
         B     XIT                                                              
         EJECT                                                                  
*              RETURNS A DEMO VALUE FROM NETBLOCK                               
         SPACE 3                                                                
*              ARGUMENTS           1 (R6) DEMO NUMBER REQUESTED                 
*                                    (R3) A(NET DEMO BLOCK)                     
*                                  2      E=EST A=ACTUAL DEMOS                  
*                                    (R5) A(4-BYTE OUTPUT AREA)                 
*                                                                               
*              INPUT               R9=A(NETBLOCK)                               
*                                                                               
*              LOCALS              R2=A(ACT OR EST DEMOS START)                 
*                                  R4=A(THIS DEMO IN NDACTDEM/NDESTDEM)         
*                                  BYTE=MODIFIER OF REQUESTED DEMO              
*                                  HALF=FIRST BYTE=ARG2 BYTE1                   
         SPACE 1                                                                
VVGETDEM L     R3,0(R1)            SAVE ARGS                                    
         USING NDDEMBLK,R3                                                      
         L     R5,4(R1)                                                         
         ZIC   R6,0(R1)                                                         
         SPACE 1                                                                
         XC    0(4,R5),0(R5)       CLEAR OUTPUT AREA                            
         MVC   HALF(1),4(R1)       SAVE 'E' OR 'A'                              
         SPACE 1                                                                
         NETGO NVDEMTYP,DMCB,((R6),0(R3)),BYTE    GET MODIFIER                  
         SPACE 1                                                                
         TM    NBINDS6,NBI6XDEM    EXPANDED 50 DEMOS ?                          
         BO    VGDEXPDM                                                         
         CLI   HALF,C'E'           TEST ESTIMATED OR ACTUAL                     
         BNE   VVGD2                                                            
         LA    R2,NDESTDEM         ESTIMATED DEMOS                              
         CLI   BYTE,C'U'                                                        
         BNE   VVGDTYP                                                          
         OC    NDAUBLOK,NDAUBLOK   DOES UNIV BLOCK EXIT                         
         BZ    XITVVG                                                           
         L     R4,NDAUBLOK                                                      
         USING NDUNBLOK,R4                                                      
         LA    R2,NDUEDEM          ESTIMATED UNIVERSES                          
         B     VVGDTYP                                                          
         SPACE 1                                                                
VVGD2    CLI   HALF,C'A'           ACTUAL DEMOS                                 
         BE    VVGD4                                                            
         DC    H'0'                CRASH IF ARG NOT GIVEN                       
         SPACE 1                                                                
VVGD4    LA    R2,NDACTDEM         ACTUAL DEMOS                                 
         CLI   BYTE,C'U'                                                        
         BNE   VVGDTYP                                                          
         OC    NDAUBLOK,NDAUBLOK   DOES UNIV BLOCK EXIT                         
         BZ    XITVVG                                                           
         L     R4,NDAUBLOK                                                      
         LA    R2,NDUADEM          ACTUAL UNIVERSES                             
         B     VVGDTYP                                                          
         DROP  R3,R4                                                            
*                                  EXPANDED DEMOS (50)                          
VGDEXPDM DS    0H                                                               
         USING XDDEMBLK,R3                                                      
         CLI   HALF,C'E'           TEST ESTIMATED OR ACTUAL                     
         BNE   XVGD2                                                            
         LA    R2,XDESTDEM         ESTIMATED DEMOS                              
         CLI   BYTE,C'U'                                                        
         BNE   VVGDTYP                                                          
         OC    XDAUBLOK,XDAUBLOK   DOES UNIV BLOCK EXIT                         
         BZ    XITVVG                                                           
         L     R4,XDAUBLOK                                                      
*                                                                               
         USING NDUNBLOK,R4       !!! ONLY SUPPORTS 20 UNIVERSES !!!             
*                                                                               
         LA    R2,NDUEDEM          ESTIMATED UNIVERSES                          
         B     VVGDTYP                                                          
         SPACE 1                                                                
         LA    R2,XDESTDEM         ESTIMATED DEMOS                              
         CLI   BYTE,C'U'                                                        
         BNE   VVGDTYP                                                          
         OC    XDAUBLOK,XDAUBLOK   DOES UNIV BLOCK EXIT                         
         BZ    XITVVG                                                           
         L     R4,XDAUBLOK                                                      
         USING NDUNBLOK,R4                                                      
         LA    R2,NDUEDEM          ESTIMATED UNIVERSES                          
         B     VVGDTYP                                                          
         SPACE 1                                                                
XVGD2    CLI   HALF,C'A'           ACTUAL DEMOS                                 
         BE    XVGD4                                                            
         DC    H'0'                CRASH IF ARG NOT GIVEN                       
         SPACE 1                                                                
XVGD4    LA    R2,XDACTDEM         ACTUAL DEMOS                                 
         CLI   BYTE,C'U'                                                        
         BNE   VVGDTYP                                                          
         OC    XDAUBLOK,XDAUBLOK   DOES UNIV BLOCK EXIT                         
         BZ    XITVVG                                                           
         L     R4,XDAUBLOK                                                      
         LA    R2,NDUADEM          ACTUAL UNIVERSES                             
         B     VVGDTYP                                                          
         DROP  R4                                                               
         EJECT                                                                  
VVGDTYP  LR    R4,R6                                                            
         CLI   BYTE,C'U'           TEST UNIVERSE                                
         BNE   VVGDTYPX                                                         
         SLL   R4,2                GET OFFSET OF THIS DEMO                      
         B     *+8                   FROM NDUEDEM OR NDUADEM INTO R4            
VVGDTYPX SLL   R4,3                GET OFFSET OF THIS DEMO                      
         LA    R4,0(R4,R2)           FROM NDESTDEM OR NDACTDEM INTO R4          
         SPACE 1                                                                
         CLI   BYTE,C'R'                                                        
         BE    VVGRAT                                                           
         CLI   BYTE,C'V'                                                        
         BE    VVGVPH                                                           
         CLI   BYTE,C'S'                                                        
         BE    VVGSHRA                                                          
         CLI   BYTE,C'U'                                                        
         BE    VVGUNIV                                                          
         CLI   BYTE,0             EXIT IF NO REQUEST                            
         BE    XITVVG                                                           
         B     VVGIMP                                                           
         SPACE 1                                                                
VVGRAT   SR    R1,R1               RATING                                       
         ICM   R1,3,2(R4)                                                       
         STCM  R1,15,0(R5)                                                      
         B     XITVVG                                                           
         SPACE 1                                                                
VVGVPH   SR    R1,R1               VPH                                          
         ICM   R1,3,0(R4)                                                       
         STCM  R1,15,0(R5)                                                      
         B     XITVVG                                                           
         SPACE 1                                                                
VVGIMP   MVC   0(4,R5),4(R4)       IMP                                          
         B     XITVVG                                                           
         SPACE 1                                                                
VVGUNIV  MVC   0(4,R5),0(R4)       UNIV                                         
         B     XITVVG                                                           
         SPACE 1                                                                
VVGSHRA  CLI   HALF,C'E'                                                        
         BE    VVSHRE                                                           
         SR    R1,R1                                                            
         ICM   R1,3,NBACTSHR       ACTUAL SHARE                                 
         STCM  R1,15,0(R5)                                                      
         B     XITVVG                                                           
         SPACE 1                                                                
VVSHRE   SR    R1,R1                                                            
         ICM   R1,3,NBESTSHR       ESTIMATED SHARE                              
         STCM  R1,15,0(R5)                                                      
         B     XITVVG                                                           
         SPACE 1                                                                
XITVVG   B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              PRINT OUT IMPS OR POINTS                                         
         SPACE 3                                                                
*              PARAMETERS (R2)     1 BYTE 1   DEMO NUMBER IN NDDEMS             
*                                             OR DEMO TYPE IF BYTES             
*                                             2-4 ARE ZERO                      
*                         (R3)       BYTE 2-4 A(NET DEMO BLOCK)                 
*                         (R4)     2          A(FULLWORD DEMO VALUE)            
*                         (R6)     3 BYTE 1   LENGTH OF OUTPUT                  
*                                             (DEFAULT IS 7) ELSE 9             
*                         (R5)       BYTE 2-4 A(OUTPUT AREA)                    
*                                                                               
*              LOCALS              BYTE                                         
*                                                                               
*              NOTES               IMPS IN (000) OR..                           
*                                  IMPS IN (000) WITH 1 DEC PLACE               
*                                  IMPS WITH M (MILLIONS)                       
*                                  POINTS 1 DEC PLACE                           
         SPACE 1                                                                
VVPRDEM  DS    0H                                                               
         LM    R3,R5,0(R1)         SAVE ARGS                                    
         ZIC   R2,0(R1)                                                         
         ZIC   R6,8(R1)                                                         
         USING NDDEMBLK,R3                                                      
         MVC   0(7,R5),BLANKS      CLEAR OUTPUT AREA                            
         MVC   BYTE,0(R1)          OPTIONALLY PASS DEMO CATEGORY                
         OC    1(3,R1),1(R1)                                                    
         BZ    VVPRDEM2                                                         
         NETGO NVDEMTYP,DMCB,((R2),0(R3)),BYTE    GETS DEMO TYPE                
         SPACE 1                                                                
VVPRDEM2 CLI   BYTE,C'R'                                                        
         BE    VVPRAT                                                           
         CLI   BYTE,C'S'           USE RTG LOGIC FOR SHARE                      
         BE    VVPRAT                                                           
         CLI   BYTE,0             EXIT IF NO DEMO CATEGORY                      
         BE    XITPRDEM                                                         
         B     VVPOTH                                                           
         SPACE 1                                                                
VVPRAT   LTR   R6,R6                                                            
         BNZ   VVPRLEN9                                                         
         EDIT  (4,0(R4)),(7,0(R5)),1,ZERO=BLANK   RATING                        
         CLI   0(R5),C' '               USE (000) IF SPACE                      
         BE    XITPRDEM                                                         
         ICM   R1,15,0(R4)             OTHERWISE DROP DECIMAL                   
         AH    R1,=H'5'                                                         
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
         EDIT  (R1),(7,0(R5))                                                   
         B     XITPRDEM                                                         
         SPACE 1                                                                
VVPRLEN9 DS    0H                                                               
         CLI   NBPREOPT,C'Y'             CABLE PRECISION                        
         BNE   VVPRLN10                                                         
                                                                                
* LET INDIVIDUAL APPS HANDLE THIS (EFF AND SCHED DO SO NOW)                     
**       CLI   NBPOSTYP,C'N'             FOR NETWORK                            
**       BE    *+12                                                             
**       CLI   NBPOSTYP,C'S'             AND SYNDICATION                        
**       BNE   VVPRLN9C                                                         
**       SR    R0,R0                     NOT RETURNED TO 2 DEC                  
**       ICM   R1,15,0(R4)               SO EXTEND TO 2 DEC                     
**       M     R0,=F'10'                                                        
*8       STCM  R1,15,0(R4)                                                      
                                                                                
VVPRLN9C EDIT  (4,0(R4)),(9,0(R5)),2,ZERO=BLANK                                 
         B     XITPRDEM                                                         
VVPRLN10 EDIT  (4,0(R4)),(9,0(R5)),1,ZERO=BLANK                                 
         B     XITPRDEM                                                         
         SPACE 1                                                                
VVPOTH   LTR   R6,R6                                                            
         BNZ   VVPOLEN9                                                         
         L     R1,0(R4)                                                         
         CLI   BYTE,C'V'                                                        
         BE    VVPOTH4                                                          
         CLI   NBHUNOPT,C'Y'       CHECK FOR HUNDRED OPTION                     
         BNE   VVPOTH4                                                          
         CLC   NVGDNET(3),=C'ABC'                                               
         BE    VVPOTH2                                                          
         CLC   NVGDNET(3),=C'CBS'                                               
         BE    VVPOTH2                                                          
         CLC   NVGDNET(3),=C'NBC'                                               
         BE    VVPOTH2                                                          
*                                  1 DEC PLACE IF NOT NETWORK                   
         EDIT  (R1),(7,0(R5)),1,ZERO=BLANK                                      
         TM    NBINDS3,NBI3A2DC           IF 2 DEC AGY                          
         BO    XITPRDEM                                                         
         CLI   NVGDNET,0                                                        
         BNE   VVPOTH1                                                          
         CLC   4(3,R5),=C'0.0'                                                  
         BE    VVPOTH2                                                          
         SPACE 1                                                                
VVPOTH1  CLI   0(R5),C' '          UNLESS ITS TOO BIG                           
         BE    XITPRDEM                                                         
         SPACE 1                                                                
VVPOTH2  SR    R0,R0               GET THINGS INTO THOUSANDS                    
         AH    R1,=H'5'                                                         
         D     R0,=F'10'                                                        
         SPACE 1                                                                
VVPOTH4  EDIT  (R1),(7,0(R5)),ZERO=BLANK                                        
         CLI   0(R5),C' '          USE (000) IF SPACE                           
         BE    XITPRDEM                                                         
         AH    R1,=H'500'                                                       
         SR    R0,R0                                                            
         D     R0,=F'1000'                                                      
         EDIT  (R1),(6,0(R5))                                                   
         MVI   6(R5),C'M'                                                       
         B     XITPRDEM                                                         
         SPACE 1                                                                
VVPOLEN9 EDIT  (4,0(R4)),(9,0(R5)),ZERO=BLANK                                   
         CLI   BYTE,C'V'                                                        
         BE    XITPRDEM                                                         
         CLI   NBHUNOPT,C'Y'       IF HUNDREDS ARE ACTIVE                       
         BNE   XITPRDEM            SHOW TO 1 DEC PLACE                          
         EDIT  (4,0(R4)),(9,0(R5)),1,ZERO=BLANK                                 
         CLC   NVGDNET(3),=C'ABC'                                               
         BE    VVPOLN90                                                         
         CLC   NVGDNET(3),=C'CBS'                                               
         BE    VVPOLN90                                                         
         CLC   NVGDNET(3),=C'NBC'                                               
         BE    VVPOLN90                                                         
         CLI   NVGDNET,0                                                        
         BNE   XITPRDEM                                                         
         CLC   6(3,R5),=C'0.0'     UNLESS ITS NETWORK                           
         BNE   XITPRDEM                                                         
         CLI   NBPOSTYP,C'N'       PXZ MAR31/92                                 
         BE    VVPOLN90                                                         
         CLI   NBPOSTYP,C'S'                                                    
         BNE   XITPRDEM                                                         
         SPACE 1                                                                
VVPOLN90 EDIT  (4,0(R4)),(10,0(R5)),ZERO=BLANK                                  
         MVI   9(R5),C' '          SHOW IN THOUSANDS                            
         SPACE 1                                                                
XITPRDEM B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              PRINT OUT CPP/CPM                                                
         SPACE 3                                                                
*              ARGUMENTS           1 (R2)  DEMO NUMBER IN NDDEMS                
*                                    (R3)  A(NET DEMO BLOCK)                    
*                                  2 (R4)  A(FULLWORD DEMO VALUE)               
*                                  3 (R5)  A(OUTPUT AREA)                       
*                                  4       NON ZERO=DOLLARS                     
*                                              ZERO=PENNIES                     
*                                    (R6)  A(FULLWORD COST)                     
*                                                                               
*              OUTPUT              7 BYTE AREA FILLED WITH EITHER               
*                                  123.45 OR $12.34 OR $12345                   
         SPACE 1                                                                
VVPRCPM  LM    R3,R6,0(R1)         SAVE ARGS                                    
         ZIC   R2,0(R1)                                                         
         CLI   12(R1),0            IF PARA4 BYTE 1 IS NOT ZERO                  
         BE    *+8                 CONVERT TO PENNIES                           
         MH    R6,=H'100'                                                       
         USING NDDEMBLK,R3                                                      
         MVC   0(7,R5),BLANKS      CLEAR OUTPUT AREA                            
         LTR   R4,R4               XIT IF EITHER DEMO                           
         BZ    CPMXIT                                                           
         LTR   R6,R6               OR COST IS ZERO                              
         BZ    CPMXIT                                                           
         NETGO NVDEMTYP,DMCB,((R2),0(R3)),BYTE    GETS DEMO TYPE                
         CLI   BYTE,C'R'                                                        
         BE    CPP                                                              
         CLI   BYTE,C'S'           NOT VALID FOR SHARE                          
         BE    CPMXIT                                                           
         CLI   BYTE,0              XIT IF NO DEMO CATEGORY                      
         BE    CPMXIT                                                           
         B     CPM                                                              
         SPACE 1                                                                
CPP      LR    R1,R6               GET COST INTO R1 (PENNIES)                   
         M     R0,=F'20'           ADJUST FOR 1 DEC IN POINTS                   
         B     CPM2                                                             
         SPACE 1                                                                
CPM      LR    R0,R6               GET COST INTO R1 (PENNIES)                   
         SRDA  R0,31               (X2)                                         
         CLI   NBHUNOPT,C'Y'       IF IMPRESSIONS ARE IN HUNDREDS               
         BNE   CPM2                                                             
         M     R0,=F'10'              ADJUST COST ACCORDINGLY                   
         SPACE 1                                                                
CPM2     DR    R0,R4               COMPUTE CPP/CPM                              
         LTR   R1,R1               (ROUND)                                      
         BM    *+8                                                              
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         LR    R4,R1               (BORROW R4)                                  
         EDIT  (R1),(7,0(R5)),2,FLOAT=$                                         
         CLI   0(R5),C' '          TRYING FOR .$12.34                           
         BE    CPMXIT                                                           
         EDIT  (R1),(7,0(R5)),2                                                 
         CLI   0(R5),C' '          TRYING FOR .123.45                           
         BE    CPMXIT                                                           
         LR    R1,R4                                                            
         M     R0,=F'2'                                                         
         D     R0,=F'100'          NO GOOD SO RETURN .$12345                    
         LTR   R1,R1               (ROUND)                                      
         BM    *+8                                                              
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         LR    R4,R1               (BORROW R4)                                  
         EDIT  (R1),(7,0(R5)),FLOAT=$                                           
         SPACE 1                                                                
*CPMXIT   XIT1                                                                  
CPMXIT   B     XIT                                                              
         EJECT                                                                  
*              VALIDATE A START DATE                                            
         SPACE 3                                                                
*              OUTPUT              NBSELSTR USERQSTR                            
         SPACE 1                                                                
VVSTRDAT NETGO NVGETFLD,DMCB                                                    
         BNZ   VVSTDINP            CK FOR NO INPUT                              
         CLI   NBSELESE,0          IF EST RANGE, DATE REQUIRED                  
         BNE   VVSTDREQ                                                         
         CLI   NBSELEST,0          IF NO INPUT, EST IS REQUIRED                 
         BNZ   VVSTDXIT                                                         
         SPACE 1                                                                
VVSTDREQ MVI   ERROR,DATREQ                                                     
         B     TRAPERR                                                          
         SPACE 1                                                                
VVSTDINP GOTO1 DATVAL,DMCB,(0,FLD),NBSELSTR  VALIDATE IT                        
         OC    DMCB(4),DMCB        CHECK FOR ERROR                              
         BZ    VVSTDERR                                                         
         L     R1,ASPOOLD                                                       
         USING SPOOLD,R1                                                        
         MVC   USERQSTR,NBSELSTR                                                
         DROP  R1                                                               
         MVI   NBDONTFD,C'N'       DATES SPECIFICALLY SELECTED                  
*                                  SO NOW NETIO SHOULD FILTER                   
         SPACE 1                                                                
VVSTDXIT B     XIT                                                              
         SPACE 1                                                                
VVSTDERR MVI   ERROR,INVDATE                                                    
         B     TRAPERR                                                          
         EJECT                                                                  
*              VALIDATE AN END DATE                                             
         SPACE 3                                                                
*              PARAMETERS          1  MAX N'DAYS END-START                      
*                                                                               
*              OUTPUT              NBSELEND USERQEND                            
         SPACE 1                                                                
VVENDDAT L     R5,0(R1)        *** SAVE ARGS OF MAX DATE SPREAD                 
         NETGO NVGETFLD,DMCB                                                    
         BNZ   VVENDINP            CK FOR NO INPUT                              
         CLI   NBSELEST,0          IF NO INPUT, EST IS REQUIRED                 
         BNZ   VVEB4SCK                                                         
         MVI   ERROR,DATREQ                                                     
         B     TRAPERR                                                          
         SPACE 1                                                                
VVENDINP GOTO1 DATVAL,DMCB,(0,FLD),NBSELEND  VALIDATE IT                        
         OC    DMCB(4),DMCB        CHECK FOR ERROR                              
         BZ    VVENDERR                                                         
         L     R1,ASPOOLD                                                       
         USING SPOOLD,R1                                                        
         MVC   USERQEND,NBSELEND                                                
         DROP  R1                                                               
         MVI   NBDONTFD,C'N'       DATES SPECIFICALLY SELECTED                  
*                                  SO NOW NETIO SHOULD FILTER                   
         SPACE 1                                                                
VVEB4SCK CLC   NBSELSTR,NBSELEND   CK END IS NOT BEFORE START                   
         BNH   VVENDSOO                                                         
         MVI   ERROR,INVEBFRS                                                   
         B     TRAPERR                                                          
         SPACE 1                                                                
VVENDSOO CLI   WHEN,X'20'           IS THIS A SOON REQUEST?                     
         BNE   VVENDXX                                                          
         CLI   OFFLINE,C'Y'                                                     
         BE    VVENDMAX                                                         
         L     R3,ATWA                                                          
         USING T320FFD,R3                                                       
*****    CLI   1(R3),C'*'             DDS TERMINAL/NO LIMIT                     
*****    BE    VVENDMAX                                                         
         MVI   BYTE,0                                                           
         CLC   =C'PW',CONREC          ...IF PUP WRITER                          
         BNE   *+12                                                             
         MVI   BYTE,1                 ...SET BYTE                               
         B     VND1                                                             
         CLC   =C'PNG',CONREC         ...IF IT'S PNG                            
         BE    VVENDMAX               ...LET'EM GO                              
         CLC   =C'OVERNITE',CONREC    IF IT'S OVERNITE                          
         BE    VNDS1                                                            
         CLC   =C'PROGL',CONACT       IF IT'S PROGLIST(DIFF SCREEN)             
         BE    VNDS1                                                            
         CLC   =C'TRANSMIT',CONREC    IF IT'S TRANSMIT                          
         BE    VNDS1                                                            
         CLC   =C'WRITER',CONREC      OR WRITER                                 
         BNE   VND2                                                             
                                                                                
VNDS1    DS    0H                                                               
*                                                                               
VND1     DS    0H                                                               
         L     RF,=V(PERVERT)      RELOCATE ADDRESS                             
         LA    R1,RELOC                                                         
         S     R1,RELOC                                                         
         AR    RF,R1                                                            
         GOTO1 (RF),DMCB,NBSELSTR,NBSELEND                                      
******   GOTO1 =V(PERVERT),DMCB,NBSELSTR,NBSELEND,RR=RELO                       
*                                                                               
         CLC   8(2,R1),=H'350'     OVER 350 DAYS?                               
         BNH   *+8                                                              
         OI    NDINDS1,X'01'       FLAG AS LARGE REQUEST                        
*                                                                               
         CLC   =C'TH',NBSELAGY     IF ZENY                                      
         BE    *+14                                                             
         CLC   =C'PC',NBSELAGY     IF OSZRAD                                    
         BNE   VND1A                                                            
         MVI   BYTE,C'Z'                                                        
         CLC   8(2,R1),=H'744'     GETS 2 YEARS                                 
         BNH   VVENDXX                                                          
         B     VNDERRR                                                          
*                                                                               
                                                                                
VND1A    CLC   8(2,R1),=H'390'     A YEAR+                                      
         BH    VNDERRR                                                          
         B     VVENDXX            ALL AGYS GET A YEAR                           
VNDERRR  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(33),=C'*SOON REQUESTS LIMITED TO 1 YEAR*'                
         CLI   BYTE,C'Z'                                                        
         BNE   *+10                                                             
         MVC   CONHEAD+26(7),=C'2 YEARS'                                        
         GOTO1 ERREX2                                                           
*                                                                               
VNDERR   MVI   ERROR,NBINVSOO                                                   
         B     TRAPERR                                                          
*                                                                               
VND2     L     R1,ATWA                                                          
         LTR   R5,R5                                                            
         BNZ   *+8                                                              
         LA    R5,126              .THEN ITS LIMITED TO 18 WEEKS                
         CH    R5,=H'126'                                                       
         BL    VVENDMAX                                                         
         LA    R5,126                                                           
         SPACE 1                                                                
VVENDMAX LTR   R5,R5               IS THERE A MAX DATE SPREAD                   
         BZ    VVENDXX                                                          
         L     RF,=V(PERVERT)      RELOCATE ADDRESS                             
         LA    R1,RELOC                                                         
         S     R1,RELOC                                                         
         AR    RF,R1                                                            
*****    GOTO1 =V(PERVERT),DMCB,NBSELSTR,NBSELEND,RR=RELO                       
         GOTO1 (RF),DMCB,NBSELSTR,NBSELEND                                      
         MVC   HALF,8(R1)                                                       
         LH    R1,HALF                                                          
         CR    R1,R5                                                            
         BNH   VVENDXX                                                          
         MVI   ERROR,INVDTSP     EXCEEDS DATE SPREAD LIMIT                      
**       CH    R5,=H'126'        TOO MANY EXCEPTIONS                            
**       BNE   TRAPERR                                                          
**       MVI   ERROR,NBINVSOO    OR SOON REQUEST MAX                            
         B     TRAPERR                                                          
         DROP  R3                                                               
         SPACE 1                                                                
VVENDXX  BAS   RE,SETDEMOS                                                      
         B     XIT                                                              
         SPACE 1                                                                
VVENDERR MVI   ERROR,INVDATE                                                    
         B     TRAPERR                                                          
         EJECT                                                                  
*              SET DEMO LIST NOW THAT WE HAVE THE DATES                         
         SPACE 3                                                                
SETDEMOS NTR1                                                                   
         CLI   NBSELESE,0          NOT NEEDED IF NOT RANGE                      
         BNE   *+12                                                             
         CLI   NBSELEST,0          NOT NEEDED IF SPECIFIC ESTIMATE              
         BNE   XIT                                                              
         L     R3,NBADEM                                                        
         LTR   R3,R3                                                            
         BZ    XIT                                                              
         USING NDDEMBLK,R3                                                      
         TM    NDOVER,X'80'        OR IF DEMOS WERE OVERRIDDEN                  
         BO    XIT                                                              
         SPACE 1                                                                
         NETGO NVSETSPT,DMCB                                                    
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING ESTHDR,R4                                                        
         MVC   EKEYAM(3),NBACTAM                                                
         MVC   EKEYPRD,NBSELPRD                                                 
         CLC   EKEYPRD,=C'ALL'                                                  
         BNE   *+10                                                             
         MVC   EKEYPRD,=C'POL'                                                  
         MVI   EKEYEST,1                                                        
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         B     SETDEM4                                                          
         SPACE 1                                                                
SETDEM2  LA    R4,KEY              SKIP TO NEXT ESTIMATE                        
         CLI   EKEYEST,255                                                      
         BE    SETOUT                                                           
         AI    EKEYEST,1                                                        
         XC    EKEYEST+1(5),EKEYEST+1                                           
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         SPACE 1                                                                
SETDEM4  CLC   KEY(7),KEYSAVE      MATCH ON PRODUCT                             
         BNE   SETOUT                                                           
         CLI   EKEYEST+1,0         ENSURE THIS IS AN ESTIMATE                   
         BNE   SETDEM2                                                          
         ZIC   R1,EKEYEST                                                       
         LA    R2,NBESTMSK                                                      
         BRAS  RE,TESTMASK         CHECK ESTIMATE MASK                          
         BNE   SETDEM2                                                          
         MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 GETREC                                                           
         L     R4,NBAIO                                                         
         CLC   ESTART,NBSELEND     IS ESTIMATE ACTIVE FOR PERIOD                
         BH    SETDEM2                                                          
         CLC   EEND,NBSELSTR                                                    
         BL    SETDEM2                                                          
*                                                                               
         TM    NBINDS6,NBI6XDEM                                                 
         BO    SETEXPDM                                                         
*                                                                               
         MVC   NDDEMOS(60),EDEMLST     PICK UP DEMOS FROM THIS EST              
         MVC   NDWGTLST(20),EWGTLST    DEFAULT WEIGHT LIST                      
         MVC   NDWGTNAM,EWGTNM     WEIGHTED DEMO NAME                           
         MVC   NDUSRNMS,EUSRNMS    USER DEMO NAMES                              
*                                                                               
         OC    EDEM21,EDEM21       21ST DEMO?                                   
         BZ    *+10                                                             
         MVC   NDDEMOS+60(3),EDEM21                                             
         OC    EDEM21WT,EDEM21WT   21ST WEIGHT?                                 
         BZ    *+10                                                             
         MVC   NDWGTLST+20(1),EDEM21WT                                          
         B     SETOUT                                                           
         DROP  R3                                                               
SETEXPDM DS    0H                                                               
         USING XDDEMBLK,R3                                                      
         MVC   XDDEMOS(60),EDEMLST     PICK UP DEMOS FROM THIS EST              
         MVC   XDWGTLST(20),EWGTLST    DEFAULT WEIGHT LIST                      
         MVC   XDWGTNAM,EWGTNM     WEIGHTED DEMO NAME                           
         MVC   XDUSRNMS,EUSRNMS    USER DEMO NAMES                              
         OC    EDEM21,EDEM21       21ST DEMO?                                   
         BZ    SETDEM10               IF ZERO MIGHT HAVE EDEMLST1               
         MVC   XDDEMOS+60(3),EDEM21                                             
         OC    EDEM21WT,EDEM21WT   21ST WEIGHT?                                 
         BZ    *+10                                                             
         MVC   XDWGTLST+20(1),EDEM21WT                                          
         B     SETOUT                                                           
SETDEM10 DS    0H                  ASSUME WE HAVE 50                            
         MVC   XDDEMOS+60(60),EDEMLST1                                          
         MVC   XDDEMOS+120(30),EDEMLST2                                         
         MVC   XDWGTLST+20(30),EWGTLST2                                         
         B     SETOUT                                                           
*                                                                               
         SPACE 1                                                                
SETOUT   XC    FILENAME,FILENAME                                                
         NETGO NVSETUNT,DMCB                                                    
         B     XIT                                                              
         DROP  R3                                                               
         DROP  R4                                                               
         SPACE 1                                                                
*&&DO                                                                           
TESTMASK NTR1                                                                   
         SR    R0,R0                                                            
         SLDL  R0,29                                                            
         SRL   R1,29                                                            
         AR    R2,R0                                                            
         LA    R1,BITLIST(R1)                                                   
         MVC   DUB(1),0(R2)                                                     
         NC    DUB(1),0(R1)                                                     
         CLI   DUB,0                                                            
         BE    TESTNO                                                           
         SR    R0,R0                                                            
         LTR   R0,R0                                                            
         B     XIT                                                              
         SPACE 1                                                                
TESTNO   LA    R0,1                                                             
         LTR   R0,R0                                                            
         B     XIT                                                              
         SPACE 1                                                                
BITLIST  DC    X'8040201008040201'                                              
*&&                                                                             
         EJECT                                                                  
*              VALIDATE BILLING START AND END DATES                             
         SPACE 3                                                                
*              OUTPUT              NBBILSTR AND/OR NBBILEND                     
         SPACE 1                                                                
VVSTRBIL NETGO NVGETFLD,DMCB       START DATE                                   
         BZ    XIT                 OPTIONAL FIELD                               
         GOTO1 DATVAL,DMCB,(0,FLD),WORK      VALIDATE IT                        
         OC    DMCB(4),DMCB        CHECK FOR ERROR                              
         BZ    VVSTDERR                                                         
         GOTO1 DATCON,DMCB,(0,WORK),(2,NBBILSTR)                                
         B     XIT                                                              
         SPACE 3                                                                
VVENDBIL NETGO NVGETFLD,DMCB       END DATE                                     
         BZ    XIT                 OPTIONAL FIELD                               
         GOTO1 DATVAL,DMCB,(0,FLD),WORK      VALIDATE IT                        
         OC    DMCB(4),DMCB        CHECK FOR ERROR                              
         BZ    VVENDERR                                                         
         GOTO1 DATCON,DMCB,(0,WORK),(2,NBBILEND)                                
         CLI   NBBILSTR,0          IF START DATE WAS REQUESTED                  
         BE    XIT                                                              
         CLC   NBBILSTR,NBBILEND   CHECK END IS NOT BEFORE START                
         BNH   XIT                                                              
         MVI   ERROR,INVEBFRS                                                   
         B     TRAPERR                                                          
         EJECT                                                                  
*              VALIDATE PAYING START AND END DATES                              
         SPACE 3                                                                
*              OUTPUT              NBPAYSTR AND/OR NBPAYEND                     
         SPACE 1                                                                
VVSTRPAY NETGO NVGETFLD,DMCB       START DATE                                   
         BZ    XIT                 OPTIONAL FIELD                               
         GOTO1 DATVAL,DMCB,(0,FLD),WORK      VALIDATE IT                        
         OC    DMCB(4),DMCB        CHECK FOR ERROR                              
         BZ    VVSTDERR                                                         
         GOTO1 DATCON,DMCB,(0,WORK),(2,NBPAYSTR)                                
         B     XIT                                                              
         SPACE 3                                                                
VVENDPAY NETGO NVGETFLD,DMCB       END DATE                                     
         BZ    XIT                 OPTIONAL FIELD                               
         GOTO1 DATVAL,DMCB,(0,FLD),WORK      VALIDATE IT                        
         OC    DMCB(4),DMCB        CHECK FOR ERROR                              
         BZ    VVENDERR                                                         
         GOTO1 DATCON,DMCB,(0,WORK),(2,NBPAYEND)                                
         CLI   NBPAYSTR,0          IF START DATE WAS REQUESTED                  
         BE    XIT                                                              
         CLC   NBPAYSTR,NBPAYEND   CHECK END IS NOT BEFORE START                
         BNH   XIT                                                              
         MVI   ERROR,INVEBFRS                                                   
         B     TRAPERR                                                          
         EJECT                                                                  
*              DELETE HOMES FROM DEMO LIST                                      
         SPACE 3                                                                
*                                  NOTHING HAPPENS IF WEIGHTED DEMO             
*              PARAMETER           1 (R3) A(LIST OF REQUESTED DEMOS)            
*              OUTPUT              MODIFIED LIST                                
         SPACE 1                                                                
VVDELHOM L     R3,0(R1)            SAVE ARG                                     
         MVC   WORK(60),0(R3)      SAVE LIST IN WORK                            
         XC    WORK+60(3),WORK+60  ZERO TERMINATE IT                            
         LA    R4,WORK             CK FOR WEIGHTED DEMO                         
         SPACE 1                                                                
VVDHWTLP CLI   1(R4),63            CODE FOR WEIGHTED DEMO                       
         BE    XITVVDH             IF A WTED DEMO THEN XIT                      
         OC    0(3,R4),0(R4)       CK FOR EOL                                   
         BZ    VVDHDOIT                                                         
         LA    R4,3(R4)            NEXT DEMO IN LIST                            
         B     VVDHWTLP                                                         
         SPACE 1                                                                
VVDHDOIT EQU   *                   NO WEIGHTED DEMOS, DELETE HOMES              
         XC    0(60,R3),0(R3)      CLEAR ORIGINAL LIST                          
         LA    R4,WORK             R4 POINTS TO CURRENT DEMO                    
         SPACE 1                                                                
VVDHLOOP OC    0(3,R4),0(R4)       CK FOR END OF LIST                           
         BZ    XITVVDH                                                          
         CLI   1(R4),C'T'          ONLY DELETE IMPRESSIONS                      
         BE    IMPFOUND                                                         
         CLI   1(R4),C'I'                                                       
         BE    IMPFOUND                                                         
         B     NEXTDEM                                                          
         SPACE 1                                                                
IMPFOUND CLI   2(R4),1             IF HOMES                                     
         BNE   NEXTDEM                                                          
         LA    R4,3(R4)            DON'T MOVE THIS ONE BACK                     
         B     VVDHLOOP                                                         
         SPACE 1                                                                
NEXTDEM  MVC   0(3,R3),0(R4)       MOVE THIS DEMO BACK TO LIST                  
         LA    R3,3(R3)            INCREMENT BOTH POINTERS                      
         LA    R4,3(R4)                                                         
         B     VVDHLOOP                                                         
         SPACE 1                                                                
XITVVDH  B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO CREATE A WEEKLIST                                     
         SPACE 3                                                                
*              PARAMETERS          1 (R3) A(FULL WORD N'PERIODS)                
*                                  2 (R4) A(OUTPUT LIST AREA)                   
*                                  3 (R5) BYTE 1 W/M/Q                          
*                                         BYTE 2 NON ZERO = USE MONTHS          
*                                         BYTE 3 NON ZERO = USE QUARTER         
*              INPUT               NREPTYP ACCOUNTING/MEDIA                     
*                                          USED TO DETERMINE MONTH TYPE         
*                                  USES NBUSER FOR BROAD/CAL/SPECIAL            
*                                  USES 00 AND B3 PROFILES FOR SPECIAL          
*                                  NBCMPSTR NBCMPEND FOR PERIOD                 
         SPACE 1                                                                
VVWKLST  LM    R3,R5,0(R1)         SAVE ARGS                                    
         BAS   RE,WKLUTIL          ROUTINE WHICH GETS LIST                      
         MVC   NBCMPSTR,0(R4)      RESET START DATE                             
         B     XIT                                                              
         SPACE 1                                                                
*                                  WEEK LIST ROUTINE                            
*                                  ADDS BEFORE START, AFTER END                 
         SPACE 1                                                                
VVWKLBEF LM    R3,R5,0(R1)         SAVE ARGS                                    
         L     R6,0(R3)            DECREMENT NUMBER IN LIST BY 2                
         SH    R6,=H'2'                                                         
         ST    R6,0(R3)                                                         
         AH    R4,=H'4'            POINT TO 4 PAST START OF LIST                
         BAS   RE,WKLUTIL          FILL REST OF LIST                            
         L     R6,0(R3)            NEW NUMBER OF ITEMS IN LIST                  
         SLL   R6,2                LENGTH OF LIST                               
         LA    R6,0(R6,R4)         ADDRESS OF  END OF LIST                      
**->     NETGO NDADDAY,DMCB,NBCMPEND,HALF,=F'1'                                 
         GOTO1 DATCON,DMCB,(2,NBCMPEND),(0,WORK)                                
         GOTO1 ADDAY,DMCB,WORK,WORK+6,F'1'                                      
         GOTO1 DATCON,DMCB,(0,WORK+6),(2,HALF)                                  
         MVC   0(2,R6),HALF        AND PUT IN IN LIST                           
         MVC   2(2,R6),MAXDAT      PUT MAXIMUM DATE AT END                      
         SH    R4,=H'4'            RESET R4 TO START OF LIST                    
         MVC   0(2,R4),MINDAT      START IT WITH MINIMUM DATE                   
*                                  GET DAY B4 FIRST IN LIST                     
**->     NETGO NDADDAY,DMCB,4(R4),HALF,=F'-1'                                   
         GOTO1 DATCON,DMCB,(2,4(R4)),(0,WORK)                                   
         GOTO1 ADDAY,DMCB,WORK,WORK+6,F'-1'                                     
         GOTO1 DATCON,DMCB,(0,WORK+6),(2,HALF)                                  
         MVC   2(2,R4),HALF                                                     
         L     R6,0(R3)            INCREMENT NUMBER IN LIST BY 2                
         AH    R6,=H'2'                                                         
         ST    R6,0(R3)                                                         
         MVC   NBCMPSTR,MINDAT     SET START DATE                               
         B     XIT                                                              
         EJECT                                                                  
*              WORK ROUTINE CALLED ABOVE                                        
         SPACE 3                                                                
*              INPUT               R3=N'PERIODS                                 
*                                  R4=A(OUTPUT LIST)                            
*                                  R5=A(TYPE - SEE ABOVE)                       
*                                                                               
*              LOCAL               DUB=DOW/TYPE/FISC MD                         
         SPACE 1                                                                
WKLUTIL  NTR1                                                                   
         XC    DUB,DUB                                                          
         ZIC   R2,NBUSER+4         START DOW FOR BROD MON, WEEKS                
         N     R2,=XL4'0000000F'   ISOLATE NUMERIC                              
         LTR   R2,R2                                                            
         BNZ   VVWK2                                                            
         LA    R2,1                DEFAULT TO MONDAY                            
         SPACE 1                                                                
VVWK2    STC   R2,DUB                                                           
         SPACE 1                                                                
PREGETL  MVC   HALF(1),0(R5)       EITHER W OR M                                
         MVC   HALF+1(1),NBUSER+3  DEFAULT TO MEDIA MONTH TYPE                  
         CLI   NREPTYP,C'A'        IF ACCTG REPORT                              
         BNE   VVWK3                 USE ACCTG MONTH TYPE                       
         MVC   HALF+1(1),NBUSER+2                                               
         SPACE 1                                                                
VVWK3    CLI   NBPEROVR,0          USE PERIOD TYPE OVERRIDE IF SET              
         BE    *+10                                                             
         MVC   HALF+1(1),NBPEROVR                                               
         BAS   RE,SPECPROF         NEED PROFILES FOR SPECIAL                    
         CLI   DUB+4,0             SHOULD BE EMPTY                              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   DUB+4(4),NBACOM     PASS COMFACS                                 
         PRINT GEN                                                              
         NETGO NDGETLST,DMCB,HALF,NBCMPSTR,NBCMPEND,0(R3),0(R4),DUB             
         PRINT NOGEN                                                            
         CLC   0(2,R4),NBCMPSTR    STARTDATE=NBCMSTR IF LIST NOT FULL           
         BE    XITVVWK                                                          
         CLI   0(R5),C'D'                                                       
         BNE   VVWK35                                                           
         MVI   0(R5),C'W'                                                       
         B     PREGETL                                                          
         SPACE 1                                                                
VVWK35   CLI   0(R5),C'W'          IF WEEK SELECTED                             
         BNE   VVWK4                                                            
         CLI   1(R5),1                AND SHOULD NOW TRY MONTHS                 
         BNE   XITVVWK                                                          
         MVI   0(R5),C'M'                                                       
         B     PREGETL                                                          
         SPACE 1                                                                
VVWK4    CLI   0(R5),C'M'           IF MONTHS SELECTED                          
         BNE   XITVVWK                                                          
         CLI   2(R5),1                AND SHOULD NOW TRY QUARTERS               
         BNE   XITVVWK                                                          
         MVI   0(R5),C'Q'                                                       
         B     PREGETL                                                          
         SPACE 1                                                                
XITVVWK  B     XIT                 PXZCHAN                                      
         SPACE 1                                                                
MINDAT   DC    X'0000'             COMPRESSED MINIMUN DATE                      
         SPACE 1                                                                
MAXDAT   DC    X'FFFF'             COMPRESSED MAXIMUM DATE                      
         EJECT                                                                  
*              ROUTINE TO GET 00 AND B3 PROFILES FOR SPECIAL                    
         SPACE 3                                                                
SPECPROF NTR1                                                                   
         CLI   HALF+1,C'S'         ONLY NEED THINGS FOR SPECIAL                 
         BNE   XIT                                                              
         XC    KEY,KEY             LOOK FOR 00 PROFILE FIRST                    
         MVC   KEY(4),=C'S000'                                                  
         MVC   KEY+4(2),NBEFFAGY                                                
         MVI   KEY+6,C'N'          (FOR MEDIA=N)                                
         MVC   KEY+7(3),NBCLICOD                                                
         L     R2,NBAIO                                                         
         XC    0(16,R2),0(R2)                                                   
         PRINT GEN                                                              
         GOTO1 NBGTPROF,DMCB,KEY,(R2),NBDM                                      
         PRINT NOGEN                                                            
         OC    0(16,R2),0(R2)                                                   
         BZ    SPECP2                                                           
         CLI   NBUSRMED,X'41'                                                   
         BL    SPECP1                                                           
         CLI   NBUSRMED,C'N'                                                    
         BE    SPECP1                                                           
         MVC   KEY+6(1),NBUSRMED   LOOK FOR MEDIA OVERRIDE                      
         GOTO1 NBGTPROF,DMCB,KEY,(C'M',(R2)),NBDM                               
         SPACE 1                                                                
SPECP1   CLI   2(R2),6             ONLY INTERESTED WITH SPECIAL PERIOD          
         BL    SPECP2                                                           
         MVC   DUB(1),8(R2)        DAY OF WEEK                                  
         MVC   DUB+1(1),2(R2)      DATE CONTROL                                 
         MVC   DUB+2(2),6(R2)      FISCAL MONTH/DAY                             
         SPACE 1                                                                
SPECP2   MVC   KEY(4),=C'S0B3'     NOW TRY FOR B3 OVERRIDE                      
         MVI   KEY+6,C'N'                                                       
         XC    0(16,R2),0(R2)                                                   
         GOTO1 NBGTPROF,DMCB,KEY,(R2),NBDM                                      
         OC    0(16,R2),0(R2)                                                   
         BZ    XIT                                                              
         CLI   NBUSRMED,X'41'                                                   
         BL    SPECP3                                                           
         CLI   NBUSRMED,C'N'                                                    
         BE    SPECP3                                                           
         MVC   KEY+6(1),NBUSRMED   LOOK FOR MEDIA OVERRIDE                      
         GOTO1 NBGTPROF,DMCB,KEY,(C'M',(R2)),NBDM                               
         SPACE 1                                                                
SPECP3   CLI   0(R2),6             ONLY INTERESTED WITH SPECIAL PERIOD          
         BL    XIT                                                              
         MVC   DUB(1),3(R2)        DAY OF WEEK                                  
         MVC   DUB+1(1),0(R2)      DATE CONTROL                                 
         MVC   DUB+2(2),1(R2)      FISCAL MONTH/DAY                             
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE A DAY                                                   
         SPACE 3                                                                
*              INPUT               R2=A(FIELD)                                  
*              PARAMETER           1  A(1BYTE FIELD)                            
*                                     X'FF'=ALL OR DAY NUMBER                   
         SPACE 1                                                                
VVDAY    L     R3,0(R1)            SAVE ARG                                     
         IFMVC R3,1,=X'FF'         DEFAULT TO X'FF'                             
         NETGO NVGETFLD,DMCB                                                    
         BZ    VVDYXIT             CK FOR NO INPUT                              
         LKUP  FLD,DAY,WORK        PUT OUTPUT OF LOOKUP IN WORK                 
         LTR   R3,R3               IF OPTIONAL ARG GIVEN                        
         BZ    VVDY2                                                            
         MVC   0(1,R3),WORK        THEN MOVE VALUE OF LOOKUP                    
         SPACE 1                                                                
VVDY2    CLI   WORK,C' '           IF NOT FOUND                                 
         BE    VVDYERR                                                          
         SPACE 1                                                                
VVDYXIT  B     XIT                                                              
         SPACE 1                                                                
VVDYERR  MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
         SPACE 1                                                                
DAYLKUP  EQU   *                   DAY LOOKUP TABLE.                            
DAYLEN1  EQU   3                   LENGTH OF KEY VALUE                          
DAYLEN2  EQU   1                   LENGTH OF RETURNED VALUE                     
         DC    CL3'ALL',XL1'FF'                                                 
         DC    CL3'M-F',XL1'00'                                                 
         DC    CL3'MON',XL1'01'                                                 
         DC    CL3'TUE',XL1'02'                                                 
         DC    CL3'WED',XL1'03'                                                 
         DC    CL3'THU',XL1'04'                                                 
         DC    CL3'FRI',XL1'05'                                                 
         DC    CL3'SAT',XL1'06'                                                 
         DC    CL3'SUN',XL1'07'                                                 
         DC    CL3'M-S',XL1'08'                                                 
         DC    CL3'VAR',XL1'09'                                                 
         DC    XL3'FFFFFF',CL1' '      END OF TABLE                             
*                                                                               
         DS    0H                                                               
         EJECT                                                                  
*              ROUTINES TO SET VALUES IN GENERAL CONTROLLER BLOCK               
*              THIS IS TO DO I/O ON SPECIFIED FILES                             
         SPACE 3                                                                
*              INPUT               FILENAME                                     
*                                                                               
*              OUTPUT              LSTATUS  LENGTH OF KEY STATUS FIELD          
*                                  DATADISP DISPLACEMENT TO FIRST ELEM.         
*                                  LKEY     LENGTH OF KEY                       
         SPACE 1                                                                
VVSETUNT MVC   LKEY,=H'20'         FOR UNIT RECORDS                             
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'27'                                                  
         B     XIT                                                              
         SPACE 1                                                                
VVSETSPT MVC   LKEY,=H'13'         FOR SPOT RECORDS                             
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'24'                                                  
         B     XIT                                                              
         SPACE 1                                                                
VVSETSTA MVC   LKEY,=H'17'         FOR STATION RECORDS                          
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'23'                                                  
         B     XIT                                                              
         SPACE 1                                                                
VVSETCT  MVC   LKEY,=H'25'         FOR CONTROL RECORDS                          
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'28'                                                  
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE ACCOUNTING FILTER                                       
         SPACE 3                                                                
*              INPUT               R2=A(FIELD HEADER)                           
*                                  ALOWABLE INPUT 1-5                           
*                                  1=CLEARED 2=UNCLEARED                        
*                                  3=BILLED  4=UNBILLED                         
*                                  5=UNALLOCATED                                
*                                                                               
*              OUTPUT              NBSELFLT (0-6) MAY HAVE BEEN SET             
*                                  PREVIOUSLY BY PRODUCT=UNALL                  
         SPACE 1                                                                
VVFILT   NETGO NVGETFLD,DMCB                                                    
         BZ    VVFLXIT             IF NO INPUT                                  
         LTR   R0,R0               CHECK NUMERIC                                
         BZ    VVFLTERR                                                         
         C     R0,=F'6'            MAKE SURE LE 6                               
         BH    VVFLTERR                                                         
         STC   R0,NBSELFLT         RETURN NUMERIC                               
         SPACE 1                                                                
VVFLXIT  B     XIT                                                              
         SPACE 1                                                                
VVFLTERR MVI   ERROR,NOTNUM                                                     
         B     TRAPERR                                                          
         EJECT                                                                  
*              EDIT OUT DEMO HEADER NAMES                                       
         SPACE 3                                                                
* VVDEMCON- RETURN A DEMO HEADER. HANDLES WEIGHTED DEMOS.                       
*  IN:   ARG1 - (R6)  BYTE 1     DEMO NUMBER IN NDDEMOS. (0-20)                 
*               (R3)  BYTE 3-4   A(NET DEMO BLOCK)                              
*        ARG2 - BYTE  BYTE 1     C=COMPRESSED FORMAT                            
*               (R4)  BYTE 3-4   A(DBLOCK) PASSED TO DEMOCON                    
*  OUT:  ARG3 - (R2)  BYTE 1     FORMAT TO RETURN DEMO HEADER.                  
*                                (SAME FORMATS AS DEMOVAL)                      
*               (R5)  BYTE 3-4   A(OUTPUT AREA)                                 
         SPACE 1                                                                
VVDEMCON LM    R3,R5,0(R1)         SAVE ARGS                                    
         USING NDDEMBLK,R3         R3 PTS TO DEMO BLOCK                         
         USING DBLOCK,R4                                                        
         ZIC   R6,0(R1)            DEMO NUMBER                                  
         MVC   BYTE,4(R1)          COMPRESSION OPTION                           
         ZIC   R2,8(R1)            RETURN FORMAT                                
*                                                                               
         C     R2,=F'10'           IF NAD DEMO REQ                              
         BNE   VDCSKIP                                                          
         MVC   DBFILE,=C'NAD'      SET FILE                                     
         MVI   DBSELMED,C'T'                                                    
         DROP  R4                                                               
*                                                                               
VDCSKIP  DS    0H                                                               
*                                                                               
         TM    NBINDS6,NBI6XDEM    NEW 50 DEMOS?                                
         BNO   VVDC2                                                            
         USING XDDEMBLK,R3                                                      
         MH    R6,=H'3'            EACH DEMO IS 3-BYTES                         
         LA    R6,XDDEMOS(R6)                                                   
         B     VVDC5                                                            
         DROP  R3                                                               
         USING NDDEMBLK,R3                                                      
VVDC2    MH    R6,=H'3'            EACH DEMO IS 3-BYTES                         
         LA    R6,NDDEMOS(R6)      R6 PTS TO PROPER DEMO                        
         DROP  R3                                                               
VVDC5    CLI   1(R6),X'21'         IF A USER DEMO                               
         BE    VVDCUSER                                                         
         CLI   1(R6),63            IF A WEIGHTED DEMO                           
         BNE   VVDOCON                                                          
         C     R2,=F'5'            IF FORMAT 5: 5/5                             
         BNE   VVDC7                                                            
         MVC   0(5,R5),=C'WGTED'                                                
         MVC   5(5,R5),=C'*****'                                                
         B     VVDCXIT                                                          
         SPACE 1                                                                
VVDC7    TM    NBINDS6,NBI6XDEM    50 DEMOS?                                    
         BO    VVDC7B                                                           
         USING NDDEMBLK,R3                                                      
         MVC   0(7,R5),NDWGTNAM    IF FORMAT 7: 7/5                             
         B     VVDC7C                                                           
         DROP  R3                                                               
         USING XDDEMBLK,R3                                                      
VVDC7B   MVC   0(7,R5),XDWGTNAM                                                 
*                                                                               
VVDC7C   MVC   7(5,R5),=C'*WGT*'                                                
         B     VVDCU7B                                                          
         DROP  R3                                                               
         SPACE 1                                                                
VVDCUSER ZIC   R1,2(R6)            GET USER DEMO NUMBER                         
         BCTR  R1,0                DECREMENT TO CALC OFFSET                     
         MH    R1,=H'7'            USER NAMES ARE 7 BYTES                       
         TM    NBINDS6,NBI6XDEM    50 DEMOS?                                    
         BO    VVDUC1                                                           
         USING NDDEMBLK,R3                                                      
         LA    R1,NDUSRNMS(R1)     ADRESS OF USER DEMO                          
         B     VVDUC2                                                           
         DROP  R3                                                               
         USING XDDEMBLK,R3                                                      
VVDUC1   LA    R1,XDUSRNMS(R1)     ADRESS OF USER DEMO                          
*                                                                               
VVDUC2   C     R2,=F'5'            IF FORMAT 5: 5/5                             
         BNE   VVDCU7                                                           
         MVC   0(5,R5),=C'USER '                                                
         MVC   5(5,R5),=C'*****'                                                
         B     VVDCXIT                                                          
         SPACE 1                                                                
VVDCU7   MVC   0(7,R5),0(R1)        IF FORMAT 7: 7/5                            
         MVC   7(5,R5),=C'*USR*'                                                
         SPACE 1                                                                
VVDCU7B  OC    0(7,R5),BLANKS                                                   
         CLI   BYTE,C'C'           IF NOT COMPRESSED - CENTER                   
         BE    VVDOCON2                                                         
         GOTO1 CENTER,DMCB,(R5),7                                               
         B     VVDCXIT                                                          
         SPACE 1                                                                
*                                                                               
VVDOCON  CLI   2(R6),0             COMSCORE?                                    
         JNE   *+8                                                              
         LHI   R2,2                COMSCORE FORMAT                              
*                                                                               
         GOTO1 CALLOV,DMCB,0,X'D9000AE0'     A(DEMOCON)                         
*                                                                               
         OC    NBEXTEND,NBEXTEND                                                
         JZ    VDOCON0                                                          
         L     R1,NBEXTEND                                                      
         USING NBXTNDD,R1                                                       
         MVC   DMCB+16(4),NBXCDNL  A(COMSCORE DEMO NAME LIST)                   
         DROP  R1                                                               
*                                                                               
VDOCON0  L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(0,0(R6)),((R2),0(R5)),(C'S',0(R4)),0                  
         CH    R2,=H'7'                                                         
         BNE   VVDCXIT             EXTRA CHECKS FOR TYPE 7                      
         CLC   7(5,R5),=C'*****'   GET RID OF SPURIOUS *****                    
         BNE   *+10                                                             
         MVC   7(5,R5),=C'(000)'   WITH SOMETHING PRETTIER                      
         CLI   BYTE,C'C'           COMPRESSED FORMAT                            
         BNE   VVDCXIT                                                          
*                                                                               
         OC    DMCB+16(4),DMCB+16  PASSED IN COMSCORE DEMO NAMES?               
         BZ    VVDOCON1            NO - PROCEED AS USUAL                        
*                                                                               
         LA    RE,7(R5)            LAST BYTE OF RETURNED DEMO                   
         LA    R0,8                COMSCORE HAS 8 CHAR DEMO NAMES               
*                                                                               
         CLI   0(RE),C'X'          DEMO NAME HAS AN "X" IN IT?                  
         BE    VVDCXIT             YES - IT'S A COMSCORE DEMO NAME              
         BCTR  RE,0                DECREMENT INDEX                              
         BCT   R0,*-10             CONTINUE CHECKING DEMO NAME                  
*                                                                               
VVDOCON1 CLI   6(R5),X'41'         ARE WE DOWN TO 6 CHARACTERS                  
         BL    VVDCOMP6                                                         
         MVC   DUB(3),0(R6)        THEN GO FOR A 6 CHARACTER VERSION            
         MVI   DUB+1,C'T'                                                       
         GOTO1 (RF),DMCB,(0,DUB),(6,0(R5))                                      
         B     VVDCOMP6                                                         
         SPACE 1                                                                
VVDOCON2 CLI   6(R5),X'41'                                                      
         BL    VVDCOMP6                                                         
         MVC   DUB,0(R5)                                                        
         MVC   0(7,R5),BLANKS                                                   
         LR    RE,R5                                                            
         LA    RF,DUB                                                           
         LA    R0,7                                                             
         SPACE 1                                                                
VVDCOMP2 CLI   0(RF),C'-'          TAKE OUT DASHES                              
         BE    VVDCOMP4                                                         
         MVC   0(1,RE),0(RF)                                                    
         LA    RF,1(RF)                                                         
         SPACE 1                                                                
VVDCOMP4 LA    RE,1(RE)                                                         
         BCT   R0,VVDCOMP2                                                      
         SPACE 1                                                                
VVDCOMP6 MVI   6(R5),C' '                                                       
         GOTO1 CENTER,DMCB,(R5),6                                               
         SPACE 1                                                                
VVDCXIT  B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              ROUTINE TO RETURN AN NTI UNIVERSE                                
         SPACE 3                                                                
*              PARAMETERS          1 (R6) N'DEMOS IN NDDEMOS (0-20)             
*                                    (R3) A(NET DEMO BLOCK)                     
*                                  2      E=RETURN ESTIMATED UNIV               
*                                    (R4) A(DBLOCK) PASSED TO DEMOCON           
*                                  3 (R5) A(FULLWORD OUTPUT UNIVERSE)           
*                                                                               
*              LOCAL               DUB=3 BYTE DEMO EXPRESSION                   
         SPACE 1                                                                
***************************************************                             
* VVUNIV- RETURN A NTI UNIVERSE.                                                
*   HANDLES WEIGHTED AND USER DEMOS.                                            
*  IN:   ARG1 - (R6) - BYTE 1- DEMO NUMBER IN NDDEMOS. (0-19)                   
*               (R3)   BYTE 2-4 - A(NET DEMO BLOCK)                             
*        ARG2 - (R4) - A(DBLOCK) PASSED TO DEMOCON                              
*                    - BYTE 1 = E RETURN ESTIMATED UNIV                         
*  OUT:  ARG3 -                                                                 
*               (R5)   BYTE 2-4 - A(FULLWORD UNIVERSE)                          
*                                                                               
         SPACE 1                                                                
VVUNIV   LM    R3,R5,0(R1)         SAVE ARGS                                    
         USING NDDEMBLK,R3         R3 PTS TO DEMO BLOCK                         
         USING DBLOCK,R4           R4 PTS TO DEDBLOCK                           
         ZIC   R6,0(R1)            DEMO NUMBER                                  
         MH    R6,=H'3'            EACH DEMO IS 3-BYTES                         
         TM    NBINDS6,NBI6XDEM       50 DEMOS?                                 
         BO    VVUNIV3                                                          
         LA    R6,NDDEMOS(R6)      R6 PTS TO PROPER DEMO                        
         B     VVUNIV5                                                          
         DROP  R3                                                               
         USING XDDEMBLK,R3                                                      
VVUNIV3  LA    R6,XDDEMOS(R6)                                                   
         B     VVUNIV5                                                          
*        CLI   1(R6),X'21'         IF A USER DEMO                               
*        BE    VVUNUSER                                                         
*        CLI   1(R6),63            IF A WEIGHTED DEMO                           
*        BNE   *+14                                                             
*        MVC   0(4,R5),NDACTWUN                                                 
*        B     VVUNXIT                                                          
VVUNIV5  CLI   4(R1),C'E'          IF EST UNIV                                  
         BNE   VVUNCON                                                          
         SPACE 1                                                                
VVUNES   DS    0H                   ESTIMATED UNIV                              
         MVI   NBFUNCT,NBFRDHI                                                  
         L     R2,NBAIO            GET AGENCY UNIV REOCRD                       
         A     R2,=F'1000'                                                      
         USING GUVD,R2                                                          
         XC    0(50,R2),0(R2)                                                   
         XC    GUVAGY,GUVAGY                                                    
         OC    NBUNCODE,NBUNCODE   IS THERE A UNIV CODE                         
         BNZ   UNES5                                                            
         OC    NBSELSTR,NBSELSTR   IS THERE A START DATE                        
         BZ    VVUNXIT                                                          
         GOTO1 DATCON,DMCB,(0,NBSELSTR),(2,GUVDATE)                             
         B     UNES7                                                            
         SPACE 1                                                                
UNES5    MVC   GUVAGY,NBEFFAGY     AGY ALPHA                                    
         MVC   GUVCODE,NBUNCODE    UNIV CODE                                    
         SPACE 1                                                                
UNES7    L     R1,NBAIO                                                         
         ST    R1,GUVAREC          A(UNIV REC)                                  
         A     R1,=F'500'                                                       
         ST    R1,GUVAOUT          A(ELEMENT)                                   
         XC    0(168,R1),0(R1)     ************ PXZ *******                     
         MVC   GUVCMFCS,NBACOM     A(COMFACS)                                   
         SPACE                                                                  
         MVC   DMCB+4(4),=X'D9000A17'   GET A(NETWEEK)                          
         GOTO1 CALLOV,DMCB,0                                                    
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   GUVNETWK,DMCB                                                    
         SPACE                                                                  
         L     RF,=V(GETNUN)      RELOCATE ADDRESS                              
         LA    R1,RELOC                                                         
         S     R1,RELOC                                                         
         AR    RF,R1                                                            
****     GOTO1 =V(GETNUN),DMCB,(R2),RR=RELO                                     
         GOTO1 (RF),DMCB,(R2)                                                   
         CLI   GUVERROR,0                                                       
         MVC   FULL,GUVERROR                                                    
         BNE   VVUNXIT             ERROR/EXIT                                   
         XC    DBLOCK,DBLOCK       SET DBLOCK FOR DEMOUT FROM UNIVREC           
         MVC   DBFILE,=C'EVN'      GET EST DEMOS                                
         MVC   DBCOMFCS,NBACOM     A(COMFACS)                                   
         L     R1,NBAIO                                                         
         ST    R1,DBAREC              A(UNIVREC)- USE FOR  DUMMY KEY            
         LA    R1,24(R1)                                                        
         ST    R1,DBAQUART         A(1ST ELEM ON UNIV REC)- DUMMY KEY           
         L     R2,GUVAOUT          GET ELEMENT AND                              
         MVC   0(168,R1),0(R2)     SET ELEMENT TO UNIV REC                      
         SPACE 1                                                                
VVUNCON  XC    DUB,DUB             GET DEMO EXPRESSION                          
         MVC   DUB(3),0(R6)                                                     
         MVI   DUB+1,C'U'           FORCE IT TO UNIVERSE                        
         GOTO1 CALLOV,DMCB,0,X'D9000ADF'     A(DEMOUT)                          
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(C'D',DUB),(R4),0(R5)                                  
         SPACE 1                                                                
VVUNXIT  B     XIT                                                              
         SPACE 1                                                                
         DROP  R3                                                               
VVUNUSER ZIC   R1,2(R6)            GET USER DEMO NUMBER                         
         BCTR  R1,0                DECREMENT TO CALC OFFSET                     
         MH    R1,=H'4'            USER UNIVS ARE 4 BYTES EACH                  
         TM    NBINDS6,NBI6XDEM       50 DEMOS?                                 
         BO    VVUNU3                                                           
         USING NDDEMBLK,R3                                                      
         LA    R1,NDUSRUNV(R1)     ADRESS OF USER DEMO                          
         B     VVUNU5                                                           
         DROP  R3                                                               
         USING XDDEMBLK,R3                                                      
VVUNU3   LA    R1,XDUSRUNV(R1)                                                  
*                                                                               
VVUNU5   MVC   0(4,R5),0(R1)                                                    
         B     VVUNXIT                                                          
         DROP  R3,R4                                                            
         EJECT                                                                  
*              ROUTINE RETURNS DEMO TYPE (V/R/T)                                
         SPACE 3                                                                
*                                  CAN HANDLE USER DEMOS                        
*              PARAMETERS          1 (R6) DEMO NUMBER IN NDDEMOS (0-20)         
*                                    (R3) A(NET DEMO BLOCK)                     
*                                  2 (R4) A(BYTE FOR RETURN VALUE)              
         SPACE 1                                                                
VVDEMTYP BRAS RE,XVDEMTYP                                                       
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO HANDLE ACCOUNTING FILTERING ON AUNIT                  
         SPACE 3                                                                
*                                  FOR 3-IN-1 AND ESTIMATES                     
*              INPUT               NBSELFLT=ACCOUNTING FILTER 1-4               
*                                  NBCALCOS=CALCULATED COST                     
*                                                                               
*              OUTPUT              NBACTUAL=ACTUAL $ FOR THIS FILTER            
*                                  NBASSIGN=ASSIGNED                            
*                                  NBINTEG=INTEGRATION                          
*              CONDITION CODE      SET EQUAL IF UNIT ACCEPTED                   
         SPACE 1                                                                
VVACFLT  TM    NBUNITST,X'42'      IF PRE-EMPTED OR MISSED                      
         BZ    VVACF2                                                           
         XC    NBACTUAL,NBACTUAL   SET COSTS TO 0                               
         XC    NBASSIGN,NBASSIGN                                                
         XC    NBINTEG,NBINTEG                                                  
         NI    NBUNITST,X'FF'-X'20' TURN OFF ACC COST INPUT BIT                 
         SPACE 1                                                                
VVACF2   CLI   NBSELFLT,0          ORDERED                                      
         BE    VVAFORD                                                          
         CLI   NBSELFLT,1          CLEARED                                      
         BE    VVAFCLR                                                          
         CLI   NBSELFLT,2          UNCLEARED                                    
         BE    VVAFUNC                                                          
         CLI   NBSELFLT,3          BILLED                                       
         BE    VVAFBIL                                                          
         CLI   NBSELFLT,4          BILLABLE                                     
         BE    VVAFUNB                                                          
         B     VVAFOK                                                           
         SPACE 1                                                                
VVAFORD  TM    NBUNITST,X'42'      FILTER IF PRE-EMPTED OR MISSED               
         BZ    VVAFOK                                                           
         B     VVAFBAD                                                          
         SPACE 1                                                                
VVAFCLR  MVC   NBACTUAL,NBPAYTGR   REPORT ALL DOLLARS CLEARED                   
         MVC   NBASSIGN,NBPAYTGR   NOTE** NETIO HAS ADDITIONAL                  
         MVC   NBINTEG,NBPAYIGR    FILTERING ON NBSELFLT. TO GET                
         B     VVAFOK              HERE, UNIT HAS PAYING ELEMENTS.              
         SPACE 1                                                                
VVAFUNC  L     R2,NBACTUAL         ACTUAL UNCLEARED=ACTUAL-PAID.                
         S     R2,NBPAYTGR                                                      
         ST    R2,NBACTUAL                                                      
         L     R2,NBASSIGN         ACTUAL ASSIGNED=ASSIGN-PAID                  
         LTR   R2,R2                                                            
         BZ    VVAF2               ******FUDGE ***** FIX THIS                   
         S     R2,NBPAYTGR                                                      
         ST    R2,NBASSIGN                                                      
         SPACE 1                                                                
VVAF2    L     R2,NBINTEG          INTEG=INTEGCOST-INTEGPAID                    
         S     R2,NBPAYIGR                                                      
         ST    R2,NBINTEG                                                       
         LA    R0,X'12'            PAYMENT ELEMENT CODE                         
         B     VVAFCK                                                           
         SPACE 1                                                                
VVAFBIL  MVC   NBACTUAL,NBBILTGR   REPORT ALL DOLLARS PAID.                     
         MVC   NBASSIGN,NBBILTGR   NOTE** NETIO HAS ADDITIONAL                  
         MVC   NBINTEG,NBBILIGR    FILTERING ON NBSELFLT. TO GET                
         B     VVAFOK              HERE, UNIT HAS BILLING ELEMENTS.             
         SPACE 1                                                                
VVAFUNB  L     R2,NBACTUAL         ACTUAL BILLABLE=ACTUAL-BILLED                
         S     R2,NBBILTGR                                                      
         ST    R2,NBACTUAL                                                      
         L     R2,NBASSIGN         ACTUAL ASSIGNED=ASSIGN-BILLED                
         LTR   R2,R2                                                            
         BZ    VVAFU2                                                           
         S     R2,NBBILTGR                                                      
         ST    R2,NBASSIGN                                                      
         SPACE 1                                                                
VVAFU2   L     R2,NBINTEG          INTEG=INTEGCOST-INTEGBILLED                  
         S     R2,NBBILIGR                                                      
         ST    R2,NBINTEG                                                       
         LA    R0,X'10'            BILLING ELEMENT CODE                         
         B     VVAFCK                                                           
         SPACE 1                                                                
VVAFCK   OC    NBACTUAL,NBACTUAL    DONT FILTER IF ANY COSTS                    
         BNZ   VVAFOK                                                           
         OC    NBASSIGN,NBASSIGN                                                
         BNZ   VVAFOK                                                           
         OC    NBINTEG,NBINTEG                                                  
         BNZ   VVAFOK                                                           
         TM    NBUNITST,X'20'       ZERO COSTS. FILTER IF NO COST OVER          
         BZ    VVAFBAD                                                          
         GOTO1 HELLO,DMCB,(C'G',=C'UNTFILE '),((R0),NBAIO),0                    
         CLI   12(R1),0            TEST IF PREVIOUS BILLING/PAYING              
         BE    VVAFBAD             YES-SO FILTER IT OUT                         
         SPACE 1                                                                
VVAFOK   SR    R1,R1               SET STAT BIT OFF                             
         B     VVAFXIT                                                          
         SPACE 1                                                                
VVAFBAD  LA    R1,1                SET STAT BIT ON                              
         SPACE 1                                                                
VVAFXIT  LTR   R1,R1                                                            
         B     XIT                                                              
         EJECT                                                                  
*              NEW ROUTINE FOR ACCOUNTING FILTER                                
         SPACE 3                                                                
*              INPUT               NBSELFLT=ACCOUNTING FILTER 1-4               
*                                  NBCALCOS=CALCULATED COST                     
*              CONDITION CODE      SET EQUAL IF UNIT ACCEPTED                   
         SPACE 1                                                                
VVACNEW  CLI   NBSELFLT,0          ORDERED                                      
         BE    VVANOK                                                           
         CLI   NBSELFLT,1          CLEARED                                      
         BE    VVANCLR                                                          
         CLI   NBSELFLT,2          UNCLEARED                                    
         BE    VVANUNC                                                          
         CLI   NBSELFLT,3          BILLED                                       
         BE    VVANBIL                                                          
         CLI   NBSELFLT,4          BILLABLE                                     
         BE    VVANUNB                                                          
         CLI   NBSELFLT,5          UNALLOCATED COST                             
         BE    VVANNOA                                                          
         CLI   NBSELFLT,6          UNALLOCATED UNITS ONLY                       
         BE    VVANNOB                                                          
         B     VVANOK                                                           
         SPACE 1                                                                
VVANCLR  L     R1,NBACTUAL         CLEARED TESTS                                
         TM    NBUNITST,X'42'                                                   
         BZ    *+6                                                              
         SR    R1,R1                                                            
         C     R1,NBPAYTGR                                                      
         BNE   VVANBAD                                                          
         CLC   NBINTEG,NBPAYIGR                                                 
         BNE   VVANBAD                                                          
         B     VVANOK                                                           
         SPACE 1                                                                
VVANUNC  L     R1,NBACTUAL         UNCLEARED TEST                               
         TM    NBUNITST,X'42'      PREEMPTED OR MISSED                          
         BZ    VVUNC1                                                           
         SR    R1,R1                                                            
         NI    NBUNITST,X'FF'-X'20'   TURN OFF ACC COST INPUT BIT               
VVUNC1   C     R1,NBPAYTGR                                                      
         BNE   VVANOK                                                           
*        ICM   R1,15,NBSPCHRG                                                   
*        TM    NBUNITST,X'42'                                                   
*        BZ    *+6                                                              
*        SR    R1,R1                                                            
*        C     R1,NBPAYSPC                                                      
*        BNE   VVANOK                                                           
         OC    NBPAYTGR,NBPAYTGR                                                
         BNZ   VVUNC2                                                           
         L     R6,NBAIO            WAS ZERO COST PAID                           
         MVI   ELCODE,X'12'                                                     
         BAS   RE,GETEL            IS THERE A PAYING ELEMENT                    
         BE    VVANBAD             YES/REJECT                                   
*                                                                               
         TM    NBUNITST,X'20'      IS IT ZERO COST INPUT                        
         BNZ   VVANOK                                                           
VVUNC2   CLI   NBPOSTYP,C'S'       SYN DONT HAVE INTEGRATION                    
         BE    VVANBAD                                                          
         CLI   NBPOSTYP,C'C'       CABLE DONT HAVE INTEGRATION                  
         BE    VVANBAD                                                          
         CLC   NBINTEG,NBPAYIGR                                                 
         BE    VVANBAD                                                          
         B     VVANOK                                                           
         SPACE 1                                                                
VVANBIL  CLC   NBCALCOS,NBBILTGR   BILLED TESTS                                 
         BNE   VVANBAD                                                          
         CLC   NBINTEG,NBBILIGR                                                 
         BNE   VVANBAD                                                          
         B     VVANOK                                                           
         SPACE 1                                                                
VVANUNB  CLC   NBCALCOS,NBBILTGR   UNBILLED TEST                                
         BNE   VVANOK                                                           
         CLC   NBINTEG,NBBILIGR                                                 
         BE    VVANBAD                                                          
         B     VVANOK                                                           
         SPACE 1                                                                
VVANNOA  TM    NBUNITST,X'20'      NO ALLOCATED COST                            
         BZ    VVANOK                                                           
         B     VVANBAD                                                          
         SPACE 1                                                                
VVANNOB  L     R6,NBAIO            IS THERE COPYSPLIT ELEMENT                   
         MVI   ELCODE,X'14'                                                     
         BAS   RE,GETEL                                                         
         BE    VVANBAD             YES/REJECT                                   
         L     R6,NBAIO            IS THERE PRODUCT ELEMENT                     
         MVI   ELCODE,X'19'                                                     
         BAS   RE,GETEL                                                         
         BE    VVANBAD             YES/REJECT                                   
         CLI   NBPRD,0             UNALLOCATED UNITS ONLY                       
         BE    VVANOK                                                           
         B     VVANBAD                                                          
         SPACE 1                                                                
VVANOK   SR    R1,R1               SET STAT BIT OFF                             
         B     VVANXIT                                                          
         SPACE 1                                                                
VVANBAD  LA    R1,1                SET STAT BIT ON                              
         SPACE 1                                                                
VVANXIT  LTR   R1,R1                                                            
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE PROGRAM SEQUENCE OPTION                                 
         SPACE 3                                                                
*              INPUT               R2=A(PROGRAM SEQUENCE FIELD)                 
*                                                                               
*              OUTPUT              NBSEQ P FIELD IS D(DAY/TIME SEQ)             
*                                        Q FIELD IS C(PROGCODE SEQ)             
*                                        Q FIELD NOT GIVEN                      
         SPACE 1                                                                
VVPSEQ   MVI   NBSEQ,C'Q'          SET DEFAULT                                  
         NETGO NVGETFLD,DMCB                                                    
         BZ    VVPSQXIT            CK FOR NO INPUT                              
         CLI   FLD,C'C'            IF C, NBSEQ IS ALREADY SET                   
         BE    VVPSQXIT                                                         
         MVI   NBSEQ,C'P'          SET UP IF INPUT=D                            
         CLI   FLD,C'D'            IF D, EVERYTHING OK                          
         BE    VVPSQXIT                                                         
         MVI   ERROR,INVALID       UNKNOWN INPUT                                
         B     TRAPERR                                                          
         SPACE 1                                                                
VVPSQXIT B     XIT                                                              
         EJECT                                                                  
*              TITLE ROUTINES                                                   
         SPACE 3                                                                
VVTITLE  MVC   NDTITLE,BLANKS      SAVE TITLE IF INPUT                          
         NETGO NVGETFLD,DMCB                                                    
         BZ    XIT                                                              
         MVC   NDTITLE,FLD                                                      
         B     XIT                                                              
         SPACE 1                                                                
VVTITOUT L     R2,0(R1)            OPTIONAL PARAMETER OF ADDRESS                
         LA    R3,132(R2)                                                       
         LTR   R2,R2                                                            
         BNZ   VVTIT1                                                           
         L     R1,ASPOOLD                                                       
         USING SPOOLD,R1                                                        
         LA    R2,H1+45            ELSE DEFAULT TO HI COL 46                    
         LA    R3,H2+45                                                         
         DROP  R1                                                               
         SPACE 1                                                                
VVTIT1   CLC   NDTITLE,BLANKS      WAS A TITLE PROVIDED                         
         BE    VVTIT2                                                           
         MVC   0(40,R2),NDTITLE    YES SO PUT IT IN                             
         SPACE 1                                                                
*                                  CENTER IT AND UNDERLINE                      
VVTIT2   GOTO1 CENTER,DMCB,(R2),40                                              
         GOTO1 UNDERLIN,DMCB,(40,(R2)),(X'BF',(R3))                             
         B     XIT                                                              
         EJECT                                                                  
*              STANDARD HEADLINE HANDLING                                       
         SPACE 3                                                                
VVHEAD   NETGO NVTITOUT,DMCB       DEAL WITH TITLE                              
         SR    R1,R1                                                            
         CLI   NDPRGRMD,0      IF PRD GROUP ON,CHK NAME LENGTH                  
         BE    VVHEAD2                                                          
         MVC   WORK(12),NDPRGBK1   NEED TO GET LENGTH OFLONGEST NAME            
         OC    WORK(12),NDPRGBK2                                                
         LA    R2,WORK+11                                                       
         LA    R1,12                                                            
         SPACE 1                                                                
VHLOOP   CLI   0(R2),X'40'                                                      
         BH    VVHEAD2                                                          
         BCTR  R2,0                                                             
         BCT   R1,VHLOOP                                                        
         SPACE 1                                                                
VVHEAD2  L     R5,ASPOOLD                                                       
         USING SPOOLD,R5                                                        
         LA    R2,H4               R2=A(DESCRIPTION)                            
         LA    R3,H4+10            R3=A(CODE)                                   
         LA    R4,H4+18            R4=A(NAME)                                   
         DROP  R5                                                               
*                                                                               
         CHI   R1,9                IS PRDGRP NAME FORCING A SHIFT               
         BNH   VVHEAD2A                                                         
         AH    R3,=H'4'            YES/ADD TO REGS                              
         AH    R4,=H'4'                                                         
*                                                                               
VVHEAD2A CLI   NDCLIRMD,NDNONE     CLIENT DETAILS                               
         BE    VVHEAD10                                                         
         MVC   0(6,R2),=C'CLIENT'                                               
         MVC   0(3,R3),NDCLIABR                                                 
         MVC   0(20,R4),NDCLINAM                                                
         CLI   NDCLIRMD,NDALL                                                   
         BE    VVHEAD3                                                          
         L     R5,NDCLISCR         PICK UP SCREEN DETAILS                       
         BAS   RE,FROMTWA          FROM TWA                                     
         MVC   0(7,R3),WORK                                                     
         MVC   0(20,R4),WORK+10                                                 
         SPACE 1                                                                
VVHEAD3  LA    R2,132(R2)                                                       
         LA    R3,132(R3)                                                       
         LA    R4,132(R4)                                                       
         SPACE 1                                                                
         CLI   NDPRGRMD,0          PRODUCT GROUP                                
         BE    VVHEAD10                                                         
         LA    R5,NDPRGBK1+11                                                   
         LA    R1,12                                                            
LOOP2    CLI   0(R5),X'40'                                                      
         BH    XLOOP2                                                           
         BCTR  R5,0                                                             
         BCT   R1,LOOP2                                                         
         LTR   R1,R1                                                            
         BNZ   XLOOP2                                                           
         DC    H'0'                                                             
XLOOP2   BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),NDPRGBK1                                                 
         MVC   0(4,R3),NDPRGAB1                                                 
         MVC   0(24,R4),NDPRGNM1                                                
         OC    NDPRGAB2,NDPRGAB2   IS IT A TWO LEVEL BREAK                      
         BZ    VVHEAD5                                                          
         LA    R2,132(R2)                                                       
         LA    R3,132(R3)                                                       
         LA    R4,132(R4)                                                       
         LA    R5,NDPRGBK2+11                                                   
         LA    R1,12                                                            
LOOP3    CLI   0(R5),X'40'                                                      
         BH    XLOOP3                                                           
         BCTR  R5,0                                                             
         BCT   R1,LOOP3                                                         
         LTR   R1,R1                                                            
         BNZ   XLOOP3                                                           
         DC    H'0'                                                             
XLOOP3   BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),NDPRGBK2                                                 
         MVC   0(4,R3),NDPRGAB2                                                 
         MVC   0(24,R4),NDPRGNM2                                                
VVHEAD5  LA    R2,132(R2)                                                       
         LA    R3,132(R3)                                                       
         LA    R4,132(R4)                                                       
         CLI   NDPRDRMD,NDALL                                                   
         BNE   VVHEAD12                                                         
         CLI   NDPRGRMD,2                                                       
         BNE   VVHEAD12                                                         
         SPACE 1                                                                
VVHEAD10 CLI   NDPRDRMD,NDNONE     PRODUCT DETAILS                              
         BE    VVHEAD12                                                         
         MVC   0(7,R2),=C'PRODUCT'                                              
         MVC   0(3,R3),NDPRDKEY                                                 
         MVC   0(20,R4),NDPRDNAM                                                
         CLI   NDPRDRMD,NDALL                                                   
         BE    VVHEAD11                                                         
         L     R5,NDPRDSCR         PICK UP SCREEN DETAILS                       
         BAS   RE,FROMTWA          FROM TWA                                     
         MVC   0(7,R3),WORK                                                     
         MVC   0(20,R4),WORK+10                                                 
         SPACE 1                                                                
VVHEAD11 LA    R2,132(R2)                                                       
         LA    R3,132(R3)                                                       
         LA    R4,132(R4)                                                       
         SPACE 1                                                                
VVHEAD12 CLI   NDESTRMD,NDNONE     ESTIMATE DETAILS                             
         BE    VVHEAD14                                                         
         MVC   0(8,R2),=C'ESTIMATE'                                             
         MVC   0(3,R3),NDESTABR                                                 
         MVC   0(24,R4),NDESTNAM                                                
         CLI   NDESTRMD,NDALL                                                   
         BE    VVHEAD14                                                         
         L     R5,NDESTSCR         PICK UP SCREEN DETAILS                       
         BAS   RE,FROMTWA          FROM TWA                                     
         MVC   0(7,R3),WORK                                                     
         MVC   0(24,R4),WORK+10                                                 
         SPACE 1                                                                
*                                  NETWORK DETAILS                              
VVHEAD14 L     R5,ASPOOLD                                                       
         USING SPOOLD,R5                                                        
         LA    R3,H5+98                                                         
         DROP  R5                                                               
         CLI   NDNETRMD,NDNONE                                                  
         BE    VVHEAD16                                                         
         CLI   NDNETRMD,NDTOG                                                   
         BE    VVHEAD16                                                         
         MVC   0(7,R3),=C'NETWORK'                                              
         MVC   8(4,R3),NDNETKEY                                                 
         CLI   NDNETRMD,NDALL                                                   
         BE    VVHEAD15                                                         
         L     R5,NDNETSCR         PICK UP SCREEN DETAILS                       
         BAS   RE,FROMTWA          FROM TWA                                     
         MVC   8(4,R3),WORK                                                     
         SPACE 1                                                                
VVHEAD15 LA    R3,132(R3)                                                       
         SPACE 1                                                                
*                                  DAYPART DETAILS                              
VVHEAD16 CLI   NDDPTRMD,NDNONE                                                  
         BE    VVHEAD18                                                         
         CLI   NDDPTRMD,NDTOG                                                   
         BE    VVHEAD18                                                         
         MVC   0(7,R3),=C'DAYPART'                                              
         MVC   8(8,R3),NDDPTNAM                                                 
         CLI   NDDPTRMD,NDALL                                                   
         BE    VVHEAD17                                                         
         L     R5,NDDPTSCR         PICK UP SCREEN DETAILS                       
         BAS   RE,FROMTWA          FROM TWA                                     
         MVC   8(8,R3),WORK+10                                                  
         SPACE 1                                                                
VVHEAD17 LA    R3,132(R3)                                                       
*                                  PACKAGE DETAILS                              
VVHEAD18 CLI   NDDPTRMD,NDNONE                                                  
         BE    VVHEAD22                                                         
         CLI   NDDPTRMD,NDTOG                                                   
         BE    VVHEAD22                                                         
         MVC   0(7,R3),=C'PACKAGE'                                              
         MVC   WORK,BLANKS                                                      
         MVC   WORK(3),NDPAKABR                                                 
         MVC   WORK+5(36),NDPAKNAM                                              
         CLI   NDPAKRMD,NDALL                                                   
         BE    VVHEAD20                                                         
         L     R5,NDPAKSCR         PICK UP SCREEN DETAILS                       
         BAS   RE,FROMTWA          FROM TWA                                     
         SPACE 1                                                                
VVHEAD20 GOTO1 SQUASHER,DMCB,WORK,64                                            
         GOTO1 CHOPPER,DMCB,(64,WORK),(25,8(R3)),(C'P',2)                       
         SPACE 1                                                                
VVHEAD22 B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO GET INPUT AND NAME FROM TWA                           
         SPACE 3                                                                
*              INPUT               R5=A(SCREEN HEADER)                          
*              OUTPUT              INPUT AT WORK                                
*                                  NAME AT WORK+10                              
         SPACE 3                                                                
FROMTWA  NTR1                                                                   
         MVC   WORK,BLANKS                                                      
         ZIC   R1,0(R5)            R5=A(FIELD HEADER)                           
         SHI   R1,9                                                             
         BM    XIT                                                              
         TM    1(R5),X'02'                                                      
         BNO   FROMTWA2                                                         
         SHI   R1,8                                                             
         BM    XIT                                                              
         SPACE 1                                                                
FROMTWA2 CH    R1,=H'40'                                                        
         BH    XIT                                                              
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),8(R5)                                                    
         SPACE 1                                                                
         ZIC   R0,0(R5)            BUMP TO NEXT FIELD ON SCREEN                 
         AR    R5,R0                                                            
         ZIC   R1,0(R5)            R5=A(FIELD HEADER)                           
         SHI   R1,9                                                             
         BM    XIT                                                              
         TM    1(R5),X'02'                                                      
         BNO   FROMTWA4                                                         
         SHI   R1,8                                                             
         BM    XIT                                                              
         SPACE 1                                                                
FROMTWA4 CH    R1,=H'40'                                                        
         BH    XIT                                                              
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK+10(0),8(R5)                                                 
         OC    WORK,BLANKS                                                      
         B     XIT                                                              
         EJECT                                                                  
*              INITIALIZE TO RUN DRIVER                                         
         SPACE 3                                                                
*                                  LOADS PHASES                                 
*                                  SETS GLOBAL ADDRESSES                        
         SPACE 1                                                                
VVDRINIT GOTO1 CALLOV,DMCB,X'B1000000',0,0  LOAD T320B1(GLOBAL STORAGE)         
         L     R4,DMCB                     FOR DRIVER                           
         ST    R4,NDGLOBAL                                                      
         USING GLOBALD,R4                                                       
****     LR    R1,R4                                                            
****     AH    R1,DMCB+10           NEED TO UPDATE AOVERLAY                     
****     LA    R1,8(R1)             TO POINT TO NEXT FREE LOCATION              
****     SRL   R1,3                (MAKE SURE ITS ON DOUBLE WORD BOUND)         
****     SLL   R1,3                                                             
****     ST    R1,AOVERLAY                                                      
         SPACE 1                                                                
         GOTO1 CALLOV,DMCB,0,X'D9000A3A'   LOAD T00A3A (DRIVER)                 
         L     R2,DMCB                                                          
         ST    R2,NDDRIVER                                                      
         SPACE 1                                                                
         GOTO1 CALLOV,DMCB,0,X'D9000A41'   LOAD T00A41 (NETWORK DRIVER)         
         L     R2,DMCB                                                          
         ST    R2,GLASYSDR                                                      
         ST    RC,GLAWORKD                                                      
         MVI   GLTWORKD,GLTSPOOL                                                
         CLI   NDDOWNL,C'Y'         OPTION TO DOWNLOAD                          
         BNE   DRI2                                                             
         MVI   GLDOWNLD,X'80'                                                   
         SPACE 1                                                                
DRI2     B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO SET NDSFTARG AND HEADLINES FROM DICT RECS             
         SPACE 2                                                                
*        INPUT    ARG1 (R4)  BYTE 1   - MAX NUMBER OF DATA FIELDS               
*                            BYTE 2-4 - A(HEADLINE SAVE AREA)                   
         SPACE                                                                  
VVDATA   L     R4,0(R1)                                                         
         MVC   BYTE,0(R1)                                                       
         LA    R5,NDSFTARG                                                      
         NETGO NVGETFLD,DMCB                                                    
         BZ    VVDATAX                                                          
         GOTO1 SCANNER,DMCB,(R2),(10,BLOCK)                                     
         LA    R3,BLOCK                                                         
         ZIC   R0,DMCB+4                                                        
         LTR   R0,R0                                                            
         BZ    VVDATAR1                                                         
         ZIC   R1,BYTE            TEST IF INPUT LINES EXCEED MAXIMUM            
         CR    R0,R1                                                            
         BH    VVDATR2                                                          
*                                                                               
VVDTA5   ZIC   R1,0(R3)            GET LENGTH OF INPUT                          
         LTR   R1,R1                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         STC   R1,FLDH+5                                                        
         LA    R1,8(R1)                                                         
         STC   R1,FLDH                                                          
         SHI   R1,9                                                             
         LA    R2,FLDH                                                          
         XC    FLD,FLD                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),12(R3)                                                   
         BAS   RE,FILLARGS                                                      
         BNE   VVDATAX                                                          
         LA    R3,32(R3)           BUMP SCANNER OUTPUT BLOCK                    
         LA    R5,1(R5)            BUMP NDSFTARG                                
         LA    R4,24(R4)           BUMP TO NEXT HEADLINE SAVE AREA              
         BCT   R0,VVDTA5                                                        
         SR    R1,R1                                                            
         LTR   R1,R1                                                            
*                                                                               
VVDATAX  XIT1  REGS=(R1)                                                        
*                                                                               
VVDATAR1 MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
VVDATR2  MVI   ERROR,TOOMUCH                                                    
         B     TRAPERR                                                          
         EJECT                                                                  
*              ONLY CALLED FROM VVDATA                                          
*              CALLS DRONE TO  VALIDATE DATA FIELDS                             
         SPACE 2                                                                
*         EXPECTS R5-SFTARGS                                                    
*                 R4-AREA FOR SAVING FIELD HEADERS                              
*                                                                               
FILLARGS NTR1                                                                   
         OC    NDDRONE,NDDRONE                                                  
         BNZ   FRG5                                                             
         GOTO1 CALLOV,DMCB,0,X'D9000A39'  LOAD T00A39 (DRONE)                   
         MVC   NDDRONE,DMCB                                                     
FRG5     DS    0H                                                               
         L     R1,ATWA                                                          
         OC    4(2,R1),4(R1)       ON NEW SECURITY?                             
         BZ    FRG10                                                            
         MVC   DRSECBLK,NBSECSV    PASS SECRET BLOCK                            
FRG10    MVI   DRWHO,DRNETWHO                                                   
         MVI   DRACTION,DRENTRY                                                 
         MVC   DRDICT(4),=C'NETA'                                               
         L     R1,NBAIO                                                         
         ST    R1,DRNETIO                                                       
         OC    NBACOM,NBACOM                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   DRCOMFAC,NBACOM                                                  
         ST    R2,DRNETFLD                                                      
         LA    R2,DRGEN                                                         
         GOTO1 NDDRONE,DMCB,(R2)                                                
         CLI   DRERROR,0                                                        
         BNE   FRGERR                                                           
         L     R6,DRNETIO                                                       
         LA    R6,42(R6)                                                        
         MVI   ELCODE,X'22'                                                     
         BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         USING DEITD,R6                                                         
         CLC   DEITYPE,=C'M+'      CHECK DATA TYPE                              
         BNE   FRGERR                                                           
         MVI   ELCODE,X'25'                                                     
         BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         USING DEIAD,R6                                                         
         MVC   0(1,R5),DEIARGS        SET SOFT DOLLAR ARG                       
         MVI   ELCODE,X'87'                                                     
         BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         USING DELITD,R6                                                        
MOVHED   DS    0H                                                               
         LA    R2,2                                                             
         ZIC   R1,DELITLEN                                                      
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),DELITRAL         SET HEADLINE                            
         BAS   RE,NEXTEL                                                        
         BNE   FRGX                                                             
         LA    R4,12(R4)                                                        
         BCT   R2,MOVHED                                                        
         DC    H'0'                SHOULD ONLY BE TWO HEADS                     
FRGX     DS    0H                                                               
         SR    R1,R1                                                            
         LTR   R1,R1                                                            
         B     XIT                                                              
         DROP  R6                                                               
*                                                                               
FRGERR   DS    0H                                                               
         LA    R4,10                                                            
         LA    R3,NDSFTARG                                                      
         SR    R1,R1                                                            
FRGERR5  CLI   0(R3),0                                                          
         BE    FRGERR7                                                          
         LA    R3,1(R3)                                                         
         LA    R1,1(R1)                                                         
         BCT   R4,FRGERR5                                                       
FRGERR7  LA    R1,1(R1)                                                         
         LTR   R1,R1                                                            
         XIT1  REGS=(R1)                                                        
         EJECT                                                                  
*********************************************                                   
* SETS UP DRIVER BLOCK SO NEW SPOOL PROGRAMS                                    
* CAN USE HEADLINE ROUTINE TO FILL IN PRODGROUPS                                
* ALSO SETS DRIVER BLOCK SO HEADLINE ROUTINE IGNORES                            
* ALL EXCEPT PRODGROUP                                                          
*                                                                               
*              INPUT               R2 = A(3-BYTE PRODUCT GROUP CODE)            
*                                                                               
VVPRDPGE DS    0H                                                               
         MVC   NDPRGKEY,0(R2)      3-BYTE CODE                                  
*                                  NEED FAIR BIT HERE                           
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PRGKEY,R4                                                        
         MVC   PRGKTYP,=X'0D01'                                                 
         MVC   PRGKAGMD,NDAGYKEY                                                
         MVC   PRGKCLT,NDCLIKEY                                                 
         MVC   PRGKID,0(R2)              GET PROG GRP DEF REC                   
*                                                                               
         NETGO NVSETSPT,DMCB                                                    
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 GETREC                                                           
*                                                                               
         L     R4,NBAIO                                                         
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PRGEL01,R4                                                       
         MVI   NDPRGNLV,1                                                       
         MVC   NDPRGBK1,PRGBK1                                                  
         CLI   PRGBK2,0                                                         
         BE    PGC5                                                             
         MVI   NDPRGNLV,2                                                       
         MVC   NDPRGBK2,PRGBK2                                                  
PGC5     MVC   HALF(1),PRGBK1LN           SAVE DIGIT LENGTH OF LEVELS           
         MVC   HALF+1(1),PRGBK2LN                                               
*                                                                               
         LA    R4,KEY                                                           
         USING PRGKEY,R4                                                        
         MVC   PRGKID(3),0(R2)               GET SPECIFIC PROG GRP REC          
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(6),KEYSAVE                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 GETREC                                                           
         L     R4,NBAIO                                                         
         XC    FULL,FULL                                                        
         MVC   NDPRGAB1(1),PRGKID                                               
         MVC   FULL+1(2),PRGKGRP                                                
         MVI   FULL+3,X'0C'          ADD X'0C' SINCE PRGKGRP IS PWOS            
         UNPK  WORK(5),FULL+1(3)                                                
         LA    R5,NDPRGAB1+1                                                    
         ZIC   R1,HALF                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),WORK      ** NOTE DO NOT TOUCH WORK NEEDED BELOW         
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PRGEL10,R4                                                       
         MVC   NDPRGNM1,PRGNAM1                                                 
         ZIC   R1,HALF             IS THERE A SECOND LEVEL                      
         ZIC   R5,HALF+1                                                        
         LTR   R5,R5                                                            
         BZ    PGCX                                                             
         AR    R1,R5                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   NDPRGAB2+1(0),WORK      SET CODE(** NOTE WORK HAS CODE)          
         MVC   NDPRGAB2(1),NDPRGAB1    SET SCHEME LETTER                        
         MVC   NDPRGNM2,PRGNAM2                                                 
*                                                                               
PGCX     DS    0H                                                               
         MVI   NDCLIRMD,0          SET SO ALL EXCEPT PRDGRP IGNORED             
         MVI   NDPRDRMD,0          IN HEADLINE ROUTINE                          
         MVI   NDESTRMD,0                                                       
         MVI   NDNETRMD,0                                                       
         MVI   NDDPTRMD,0                                                       
         B     XIT                                                              
         EJECT                                                                  
*  2 AND 8 CHARACTER PERSONAL USER ID                                           
VVUSERID DS    0H                                                               
         BRAS  RE,GTUSERID                                                      
         B     XIT                                                              
         EJECT                                                                  
*  ADD AND OR MAINTAIN UNIT ACTIVITY ELEMENT                                    
*  ASSUMED UNIT RECORD STORED IN NBAIO                                          
VVBDACTY DS    0H                                                               
         BRAS  RE,GTACTY                                                        
         B     XIT                                                              
         EJECT                                                                  
*  CHECK FOR VALID REASON CODE                                                  
VVCHKRSN DS    0H                                                               
         L     R3,0(R1)            POINT TO REASON CODE                         
         CLC   0(4,R3),BLANKS                                                   
         BNH   CHKRSNX                                                          
         LA    R4,KEY                                                           
         USING RSNRECD,R4                                                       
         XC    KEY,KEY                                                          
         MVC   RSNKTYPE,=X'0D77'                                                
         MVC   RSNKAGY,AGENCY                                                   
         MVI   RSNKMED,C'N'                                                     
         MVC   RSNKCODE(4),0(R3)                                                
         OC    RSNKCODE,BLANKS                                                  
         MVC   FILENAME,=CL8'SPTDIR'                                            
         GOTO1 HIGH                                                             
                                                                                
         CLC   KEY(9),KEYSAVE      TEST IF SAME RECORD APPLIES                  
         BE    CHKRSNX             YES                                          
         MVI   ERROR,INVREASN                                                   
         B     TRAPERR                                                          
CHKRSNX  B     XIT                                                              
         DROP  R4                                                               
*****************************************************************               
         EJECT                                                                  
*              ROUTINE TO SET DEMO OPTIONS FOR NETVALUE                         
         SPACE 3                                                                
VVDEMOPT MVI   NBSELUOP,C'A'                                                    
         MVI   NBESTOPT,C'A'                                                    
         B     XIT                                                              
         SPACE 1                                                                
TRAPERR  GOTO1 ERREX                                                            
         SPACE 1                                                                
XIT      XIT1                                                                   
         SPACE 1                                                                
         GETEL (R6),DATADISP,ELCODE                                             
         EJECT                                                                  
*              CONSTANTS & LTORG                                                
         SPACE 3                                                                
RELO     DS    A                                                                
EFFEFFS  DC    X'FFFFFFFF'         USED BY LKUP MACRO                           
         SPACE 1                                                                
         LTORG                                                                  
*                                                                               
BLANKS   DC    CL132' '                                                         
*                                                                               
*                                                                     *         
*        GET COMSCORE TOKEN RECORD                                    *         
*        ON EXIT - DMCB PARAM5 BUILT FOR DEMOVAL                      *         
*                                                                     *         
GETTOKEN NTR1  BASE=*,LABEL=*                                                   
         MVC   HALF,AGENCY         SAVE OFF THE AGENCY ALPHA                    
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CT5KEY,R4                                                        
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,AGENCY                                                  
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD '),=C'CTFILE ',KEY,AIO2                
         L     R4,AIO2                                                          
         LA    R5,CT5DATA          POINT TO THE FIRST ELEMENT IN REC            
         USING CTSEAD,R5                                                        
         CLI   0(R5),CTSEAELQ      ALREADY ON SECURITY ALPHA ELEM?              
         JE    GTOKEN10            YES                                          
         MVI   ELCODE,CTSEAELQ     NO, FIND SECURITY ALPHA ELEM                 
         BRAS  RE,NEXTEL                                                        
         JNE   *+10                                                             
GTOKEN10 MVC   HALF,CTSEAAID       GET SECURITY AGENCY ALPHA AS WELL            
         DROP  R5                                                               
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING TOKRECD,R4          BUILD TOKEN RECORD KEY                       
         MVI   TOKKMIN,TOKKMINQ                                                 
         MVI   TOKKTYP,TOKKRTRK                                                 
         MVC   TOKKSAGY,HALF       SECURITY AGENCY ALPHA                        
         MVC   TOKKAAGY,AGENCY     AND AGENCY ALPHA                             
         MVI   TOKKSYS,X'03'       NET SYSTEM                                   
         MVC   KEYSAVE,KEY                                                      
                                                                                
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI '),=C'GENDIR ',KEY,KEY                 
         CLC   KEY(L'TOKKEY),KEYSAVE                                            
         JNE   GTOKENX                                                          
                                                                                
         L     R4,AIO2                                                          
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'GENFIL ',KEY+36,(R4),WORK             
                                                                                
         AHI   R4,TOKFIRST                                                      
         USING RTAUTHD,R4                                                       
         CLI   0(R4),RTAUTELQ      X'0A' ELEMENT                                
         JNE   GTOKENX                                                          
         OC    RTAUTID(L'RTAUTID+L'RTAUTSEC),RTAUTID                            
         JZ    GTOKENX                                                          
*                                                                               
         L     RF,AIO2                                                          
         USING P5XD,RF                                                          
         XC    P5XID(P5XDLNQ),P5XID                                             
         MVC   P5XID,=C'P5X '                                                   
         LA    RE,RTAUTID          USER TOKEN FOR DEMOVAL                       
         ST    RE,P5XLICNS                                                      
         ST    RF,DMCB+16          PARAM5 EXTEND BLOCK                          
         OI    DMCB+16,X'80'                                                    
*                                                                               
GTOKENX  J     XIT                                                              
         DROP  R4,RF                                                            
         LTORG                                                                  
*                                                                               
TESTMASK NTR1  BASE=*,LABEL=*                                                   
         SR    R0,R0                                                            
         SLDL  R0,29                                                            
         SRL   R1,29                                                            
         AR    R2,R0                                                            
         LA    R1,BITLIST(R1)                                                   
         MVC   DUB(1),0(R2)                                                     
         NC    DUB(1),0(R1)                                                     
         CLI   DUB,0                                                            
         BE    TESTNO                                                           
         SR    R0,R0                                                            
         LTR   R0,R0                                                            
         J     XIT                                                              
         SPACE 1                                                                
TESTNO   LA    R0,1                                                             
         LTR   R0,R0                                                            
         J     XIT                                                              
         SPACE 1                                                                
BITLIST  DC    X'8040201008040201'                                              
*                                                                               
TRANS21  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,SPCGRTAB                                                      
         LHI   R0,(SPCGRTBX-SPCGRTAB)/3                                         
*                                                                               
TRANS21A CLC   FLD(2),0(R1)                                                     
         BE    TRANS21B                                                         
         LA    R1,3(R1)                                                         
         BCT   R0,TRANS21A                                                      
         MVI   ERROR,INVALID                                                    
         XIT1                                                                   
TRANS21B MVC   FLD(1),2(R1)                                                     
         XIT1                                                                   
         SPACE 1                                                                
       ++INCLUDE SPCGRTAB                                                       
         EJECT                                                                  
*************************************************                               
TARBUF   NTR1  BASE=*,LABEL=*                                                   
         L     R2,NBATBUFF         USER MUST HAVE PROVIDED THIS                 
         LTR   R2,R2                                                            
         BZ    VVTARFIN                                                         
         L     R3,NBACLI                                                        
         LTR   R3,R3               MUST HAVE CLIENT HEADER AROUND TOO           
         BZ    VVTARFIN                                                         
         NETGO NVSETSPT,DMCB       INITIALIZE FOR SPOT                          
         USING CLTHDR,R3                                                        
         LA    R3,CLIST                                                         
         USING TBUFFD,R2                                                        
         USING ESTHDR,R4                                                        
         LA    R0,220                                                           
         SPACE 1                                                                
VVTAR2   CLI   3(R3),0                                                          
         BE    VVTARLST                                                         
         XC    KEY,KEY             SET UP ESTIMATE KEY                          
         LA    R4,KEY                                                           
         MVC   EKEYAM(3),NBACTAM   SET A/M AND CLIENT IN KEY                    
         MVC   EKEYPRD,0(R3)       PRODUCT FROM CLIENT LIST                     
         MVC   EKEYEST,NBEFFEST                                                 
         XC    EKEYEST+1(5),EKEYEST+1                                           
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(7),KEYSAVE      MATCH ON PRODUCT                             
         BNE   VVTARNXT            IS GOOD ENOUGH                               
         MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 GETREC                                                           
         L     R4,NBAIO                                                         
         ZIC   R2,3(R3)            PICK UP PRODUCT NUMBER                       
         CLI   3(R3),X'FF'         POOL IS 220                                  
         BNE   *+8                                                              
         LA    R2,220                                                           
         BCTR  R2,0                                                             
***      MH    R2,=H'20'                                                        
         MH    R2,=H'25'           DEMOS INCREASED TO 25                        
         A     R2,NBATBUFF                                                      
         XC    0(20,R2),0(R2)      CLEAR ENTRY                                  
         MVC   TBUFFTAR,EDEMLST                                                 
         LA    R5,TBUFFTAR                                                      
         LA    R6,4                                                             
         SR    R1,R1                                                            
         SPACE 1                                                                
VVTAR4   OC    0(3,R5),0(R5)       COUNT N'DEMOS                                
         BZ    *+8                                                              
         LA    R1,1(R1)                                                         
         LA    R5,3(R5)                                                         
         BCT   R6,VVTAR4                                                        
         STC   R1,TBUFFNT                                                       
         SPACE 1                                                                
         LA    R5,TBUFFTAR         NOW EXPAND FIRST NAME                        
         CLI   1(R5),X'21'         CHECK FOR USER                               
         BE    VVTARUSR                                                         
         CLI   1(R5),63            OR WEIGHTED                                  
         BE    VVTARWT                                                          
         CLI   2(R5),1             OR, IF FIRST IS HOMES                        
         BNE   *+8                                                              
         LA    R5,3(R5)            SECOND DEMO                                  
         CLI   1(R5),X'21'         CHECK FOR USER                               
         BE    VVTARUSR                                                         
         CLI   1(R5),63            OR WEIGHTED                                  
         BE    VVTARWT                                                          
*                                  FILL DBLOCK FOR DEMOCON                      
         LA    R1,BLOCK                                                         
         USING DBLOCK,R1                                                        
         XC    DBLOCK,DBLOCK       OTHERWISE ITS REGULAR                        
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBAREC,NBAIO                                                     
         MVC   DBCOMFCS,NBACOM                                                  
         ST    R1,DMCB+8           PASS A(DBLOCK)                               
         MVI   DMCB+8,C'S'         PXZ                                          
         DROP  R1                                                               
         GOTO1 NBDEMCON,DMCB,(0,(R5)),(7,WORK)                                  
*        GOTO1 NBDEMCON,DMCB,(0,(R5)),(7,WORK)                                  
         MVC   TBUFFTNM,WORK                                                    
         B     VVTARNXT                                                         
         SPACE 1                                                                
VVTARUSR ZIC   R1,2(R5)            PICK UP USER DEMO NAME                       
         BCTR  R1,0                                                             
         MH    R1,=H'7'                                                         
         LA    R1,EUSRNMS(R1)                                                   
         MVC   TBUFFTNM,0(R1)                                                   
         B     VVTARNXT                                                         
         SPACE 1                                                                
VVTARWT  MVC   TBUFFTNM,EWGTNM     WEIGHTED NAME HERE                           
         SPACE 1                                                                
VVTARNXT LA    R3,4(R3)            BUMP TO NEXT PRODUCT                         
         BCT   R0,VVTAR2                                                        
         SPACE 1                                                                
VVTARLST XC    FILENAME,FILENAME                                                
         NETGO NVSETUNT,DMCB                                                    
         SPACE 1                                                                
VVTARFIN XIT1                                                                   
         DROP  R4                                                               
***************************************************                             
         EJECT                                                                  
SECRTN   NTR1  BASE=*,LABEL=*                                                   
         USING NEWSECD,R3                                                       
***      XC    NEWOFFB,NEWOFFB                                                  
***      LA    R2,NEWOFFB                                                       
         XC    NDPRGBUF,NDPRGBUF                                                
         LA    R2,NDPRGBUF                                                      
         USING OFFICED,R2                                                       
*                                                                               
         L     R4,AIO                                                           
         USING CLTHDR,R4                                                        
*                                                                               
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAUTH,NBTWAACC                                                 
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC,NBEFFOFF                                                  
         GOTO1 CLUNPK,DMCB,(CPROF+6,NBACTCLI),OFCCLT                            
         OC    OFCCLT,BLANKS                                                    
         MVC   OFCSAGMD,NBACTAM                                                 
         MVC   OFCLMT(4),NBTWAACC                                               
         MVC   OFCACCSC(3),CACCESS    ACCESS LIST FROM CLTHDR                   
         LA    RF,NEWSECA                                                       
         ST    RF,OFCSECD                                                       
*                                                                               
         MVI   ERROR,SECLOCK                                                    
         XC    DMCB(16),DMCB                                                    
*        GOTO1 AOFFICER,DMCB,(C'N',NEWOFFB),ACOMFACS                            
         GOTO1 AOFFICER,DMCB,(C'N',NDPRGBUF),ACOMFACS                           
         CLI   0(R1),0                           RETURNS CC'='/'NOT ='          
         XIT1                                                                   
         DROP  R2,R3,R4                                                         
**********************************************************                      
         EJECT                                                                  
* GET NEW SECURITY INTO TWA+7000 AND SAVE ADDR IN NETBLOCK                      
INITSEC  NTR1  BASE=*,LABEL=*                                                   
         L     R2,ATWA                                                          
         OC    4(6,R2),4(R2)    NEW SECURITY+LIMIT ACCESS                       
****     BZ    ISECX            ALWAYS USE NEWSEC                               
********   DATA SET NEMEDGEN   AT LEVEL 066 AS OF 01/25/07                      
********************************                                                
         CLI   OFFLINE,C'Y'        IF OFFLINE                                   
         BNE   INITS10                                                          
         ICM   RF,15,ACOMFACS                                                   
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     RF,CMASTC-COMFACSD(RF)                                           
         USING MCBLOCK,RF                                                       
         L     RF,MCASECRT                                                      
         ST    RF,NBSECSV                                                       
         ST    RF,ASECBLK                                                       
         DROP  RF                                                               
         B     ISECX                                                            
********************************                                                
********   DATA SET NEMEDGEN   AT LEVEL 066 AS OF 01/25/07                      
INITS10  A     R2,=F'7000'                                                      
         USING NEWSECD,R2                                                       
         ICM   RF,15,ACOMFACS                                                   
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     RF,CSECRET-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,('SECPINIT',NEWSECA),0                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R1,NEWSECA                                                       
         ST    R1,NBSECSV                                                       
         ST    R1,ASECBLK          PASS TO GENCON                               
*                                                                               
ISECX    XIT1                                                                   
         EJECT                                                                  
******************************************                                      
GTUSERID NTR1  BASE=*,LABEL=*                                                   
         GOTO1 GETFACT,DMCB,(2,0)                                               
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
*                                                                               
         TM    FATFLAG,X'08'       PASSWORD PROTECT ACTIVE?                     
         BZ    IDX                                                              
         OC    FAPASSWD,FAPASSWD   PASSWORD?                                    
         BZ    IDX                                                              
         MVC   NBUSERID,FAPASSWD                                                
         MVC   NBSECAGY,FATAGYSC                                                
         B     IDX                                                              
*  LEAVE CODE BELOW IN CASE WE EVER NEED 8 CHARACTER USERID                     
**       LA    R6,KEY                                                           
**       XC    KEY,KEY                                                          
**       USING SA0KEY,R6                                                        
**       MVI   SA0KTYP,C'0'                                                     
**       MVC   SA0KAGY,NBSELAGY                                                 
**       MVC   SA0KNUM,FAPASSWD                                                 
**       L     R6,NBAIO                                                         
**       GOTO1 DATAMGR,DMCB,(0,=C'DMREAD '),=C'CTFILE ',KEY,(R6)                
**       LA    R6,SA0DATA                                                       
**ID10     CLI   0(R6),X'C3'                                                    
**         BE    ID12                                                           
**         BH    ID13                                                           
**         ZIC   R1,1(R6)                                                       
**         LTR   R1,R1                                                          
**         BZ    ID13                                                           
**         AR    R6,R1                                                          
**         B     ID10                                                           
**       USING SAPALD,R6                                                        
**ID12     MVC   WORK(8),SAPALPID    WE PASS IT BACK WHERE?                     
**         B     IDX                                                            
**ID13     MVC   WORK+8(8),=C'WHATHPND'                                         
IDX      XIT1                                                                   
         EJECT                                                                  
***********************************************************                     
GTACTY   NTR1  BASE=*,LABEL=*                                                   
         L     R4,0(R1)            REASON CODE                                  
         XC    ELEM,ELEM                                                        
         LA    R3,ELEM             BUILD ACTIVITY ELEMENT                       
         USING NUACTD,R3                                                        
         MVI   NUACTEL,X'99'                                                    
         MVI   NUACTLEN,22                                                      
         GOTO1 DATCON,DMCB,(5,DUB),(3,NUACTADT)                                 
         MVC   NUACTCDT,NUACTADT   LAST ACTIVITY DATE                           
         MVC   NUACTAID,NBUSERID   ADD PERSONAL ID                              
         MVC   NUACTCID,NBUSERID   LAST PERSONAL ID                             
         MVC   NUACTAGD,NBSECAGY   SECURITY AGENCY                              
         MVC   NUACTRSN,0(R4)      REASON CODE                                  
         OC    NUACTRSN,=4X'40'    BLANK FILL                                   
         L     R6,NBAIO                                                         
         MVI   ELCODE,X'99'                                                     
         BAS   RE,GETEL                                                         
         BNE   ACTVTY2             NO ELEMENT FOUND                             
         MVC   NUACTACD,2(R6)      COPY OVER ADD AUTH. CODE                     
         MVC   NUACTADT,4(R6)      AND THE CREATION DATE                        
         CLI   1(RE),13                                                         
         BL    ACTVTY2                                                          
         MVC   NUACTAID,12(R6)     COPY OVER ADD PERSONAL ID                    
         MVC   NUACTAGD,16(R6)     AND SECURITY AGENCY                          
         SPACE                                                                  
ACTVTY2  L     R6,NBAIO                                                         
         GOTO1 HELLO,DMCB,(C'D',=C'UNTFILE '),(ELCODE,(R6)),0,0                 
         GOTO1 HELLO,DMCB,(C'P',=C'UNTFILE '),(ELCODE,(R6)),(R3),0              
         XIT1                                                                   
         DROP  R3                                                               
*                                                                               
         EJECT                                                                  
***********************************************************                     
STAGRP   NTR1  BASE=*,LABEL=*                                                   
         MVI   ERROR,INVALID                                                    
         LA    R4,KEY                                                           
         USING GRPRECD,R4                                                       
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D05'       STATION GROUP RECORD                       
         MVC   KEY+2(1),NBACTAM      MEDIA/AGENCY                               
         MVC   GRPKID,FLD            GROUP ID                                   
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(4),KEYSAVE                                                   
         BNE   STAGRPX             NO SUCH GROUP ID                             
         MVC   NBSELNGR,FLD          PUT ID TO NETBLOCK                         
                                                                                
* IF USER PROVIDED BUFFER/GET NETWORKS INTO TABLE                               
         CLI   OFFLINE,C'Y'       OFFLINE ONLY                                  
         BNE   STAGOK                                                           
         L     R2,NBNETGRP         BUFFER FOR STATIONS -> R2                    
         LTR   R2,R2                                                            
         BZ    STAGOK              NOT AROUND/EXIT                              
*                                                                               
         LA    R3,FLD+2            START OF SCHEME                              
         CLI   0(R3),C'-'          NEGATIVE FILTERING?                          
         BNE   *+12                                                             
         MVI   0(R2),C'-'          NEGATIVE FILTER                              
         LA    R3,1(R3)            BUMP OVER MINUS   R3->FILTER                 
         CLC   0(3,R3),=C'ALL'     IF ALL                                       
         BNE   *+10                                                             
         MVC   0(3,R3),=C'***'     MAKE IT *                                    
***      MVC   FULL(3),0(R3)       SET FILTER IN FULL                           
         MVC   FULL(4),0(R3)       SET FILTER IN FULL                           
         LA    R2,1(R2)            BUMP TO START OF STATIONS IN TABLE           
         B     STAG03                                                           
*                                                                               
NXTSTA   MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 SEQ                                                              
         CLC   KEY(4),KEYSAVE                                                   
         BNE   STAGOK                                                           
STAG03   UNPK  WORK(5),GRPKCODE(3)                                              
         LA    R3,FULL                                                          
         LA    RE,WORK             RE->GROUP CODE                               
***      LA    RF,3                                                             
         LA    RF,4                                                             
STAG05   CLI   0(R3),C'*'                                                       
         BE    STAG10                                                           
         CLI   0(R3),X'40'                                                      
         BNH   STAG10                                                           
         CLC   0(1,R3),0(RE)       IS IT A MATCH?                               
         BNE   NXTSTA              NO                                           
STAG10   LA    R3,1(R3)                                                         
         LA    RE,1(RE)                                                         
         BCT   RF,STAG05                                                        
         NETGO NVSETSPT,DMCB                                                    
         MVC   FILENAME,=C'SPTFILE '                                            
         GOTO1 GETREC                                                           
         L     R6,NBAIO                                                         
         USING GRPVALD,R6                                                       
         MVI   ELCODE,X'30'                                                     
         BAS   RE,GETEL                                                         
         BNE   NXTSTA                                                           
STAG15   MVC   0(4,R2),GRPVALUE    SET IN STATION                               
         LA    R2,4(R2)            BUMP TBL                                     
         BAS   RE,NEXTEL           ANY MORE ELEMENTS                            
         BE    STAG15                                                           
         B     NXTSTA                                                           
STAGOK   SR    R4,R4               VALID STA GROUP REQ RETURNS CC=              
*                                                                               
STAGRPX  XC    FILENAME,FILENAME                                                
         NETGO NVSETUNT,DMCB                                                    
         LTR   R4,R4                                                            
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
XPROCST  NTR1  BASE=*,LABEL=*                                                   
         L     R6,NBAIO            ESTIMATE RECORD                              
         USING ESTHDR,R6                                                        
         IFMVC R3,L'EDESC,EDESC    MOVE IF FIRST ARG GIVEN                      
         L     R1,ASPOOLD                                                       
         USING SPOOLD,R1                                                        
*                                                                               
         CLC   ELEN,=AL2(ESTHDR2Q) NEW STYLE?                                   
         JL    PEST01                                                           
         OC    ENONTDMS(200),ENONTDMS  ANY COMSCORE DEMOS?                      
         JNZ   *+14                                                             
         OC    ENONTDMS+200(200),ENONTDMS+200                                   
         JZ    PEST01                                                           
*                                                                               
         CLI   OVERLAY,X'07'       SEED REPORT?                                 
         JE    PEST01              REQSML WAS ALREADY SET IN NEMED07            
         CLI   OVERLAY,X'1B'       SEED REPORT?                                 
         JE    PEST01              REQSML WAS ALREADY SET IN NEMED07            
         MVI   REQSML,C'C'         SET IT FOR COMSCORE PASS 1/2 MODE            
*                                                                               
PEST01   MVC   USERQSTR,ESTART     SET UP DATES FOR SPOO(F/L)                   
         MVC   USERQEND,EEND                                                    
         DROP  R1                                                               
         MVC   NBSELSTR,ESTART     SET UP DATES FOR NETIO                       
         MVC   NBSELEND,EEND                                                    
         LTR   R4,R4               IF SECOND ARG GIVEN                          
         BZ    PESTXIT                                                          
         TM    NBINDS6,NBI6XDEM                                                 
         BNO   PEST10                                                           
*                                                                               
* NEW EXPANDED ESTIMATE RECORD WITH 50 DEMOS                                    
*  %%% ONLY FOR 21 NOW FOR TESTING %%%                                          
         USING XDDEMBLK,R4                                                      
         MVC   XDDEMOS(60),EDEMLST     DEFAULT DEMOS  %%%%%%%%                  
         MVC   XDWGTLST(20),EWGTLST    DEFAULT WEIGHT LIST                      
         MVC   XDWGTNAM,EWGTNM     WEIGHTED DEMO NAME                           
         MVC   XDUSRNMS,EUSRNMS    USER DEMO NAMES                              
         MVC   XDNTDMS(200),ENONTDMS    COMSCORE DEMO NAMES                     
         MVC   XDNTDMS+200(200),ENONTDMS+200                                    
*                                                                               
         OC    NBEXTEND,NBEXTEND                                                
         JZ    PEST02                                                           
         L     RF,NBEXTEND                                                      
         USING NBXTNDD,RF                                                       
         ICM   RE,15,NBXCDNL       DID CALLING PROGRAM SET NBXCDNL?             
         BZ    *+10                NO                                           
         MVC   0(160,RE),ENONTDMS  YES - SET COMSCORE DEMO NAMES                
         DROP  RF                                                               
PEST02   OC    EDEM21,EDEM21       21ST DEMO?                                   
         BZ    PEST05                 IF ZERO MIGHT HAVE EDEMLST1               
         MVC   XDDEMOS+60(3),EDEM21                                             
         OC    EDEM21WT,EDEM21WT   21ST WEIGHT?                                 
         BZ    *+10                                                             
         MVC   XDWGTLST+20(1),EDEM21WT                                          
         B     PESTXIT                                                          
PEST05   DS    0H                  ASSUME WE HAVE 50 DEMOS                      
         MVC   XDDEMOS+60(60),EDEMLST1                                          
         MVC   XDDEMOS+120(30),EDEMLST2                                         
         MVC   XDWGTLST+20(30),EWGTLST2                                         
         B     PESTXIT                                                          
         DROP  R4                                                               
*                                                                               
* OLD ESTIMATE RECORD WITH 25 DEMOS                                             
         USING NDDEMBLK,R4                                                      
PEST10   MVC   NDDEMOS(60),EDEMLST     DEFAULT DEMOS                            
         MVC   NDWGTLST(20),EWGTLST    DEFAULT WEIGHT LIST                      
         MVC   NDWGTNAM,EWGTNM     WEIGHTED DEMO NAME                           
         MVC   NDUSRNMS,EUSRNMS    USER DEMO NAMES                              
         MVC   NDNTDMS(200),ENONTDMS    COMSCORE DEMO NAMES                     
*                                                                               
         OC    EDEM21,EDEM21       21ST DEMO?                                   
         BZ    *+10                                                             
         MVC   NDDEMOS+60(3),EDEM21                                             
         OC    EDEM21WT,EDEM21WT   21ST WEIGHT?                                 
         BZ    *+10                                                             
         MVC   NDWGTLST+20(1),EDEM21WT                                          
*                                                                               
         SPACE 1                                                                
PESTXIT  XIT1                                                                   
         DROP  R6                                                               
         DROP  R4                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
XVVDEM   NTR1  BASE=*,LABEL=*                                                   
         L     R3,0(R1)            A(DBLOCK)                                    
         L     R4,4(R1)            A(NET DEMO BLOCK)                            
         USING NDDEMBLK,R4                                                      
         USING DBLOCK,R3                                                        
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'TPT'      SET UP DBLOCK                                
         MVI   DBSELMED,C'T'                                                    
         MVI   NDOVER,0                                                         
         NETGO NVGETFLD,DMCB                                                    
         BZ    VVDXIT              CK FOR NO INPUT                              
         TM    NBINDS6,NBI6XDEM    EXPANDED DEMO FLAG ?                         
         BNO   VVDEMOLD                                                         
*                                         MAX IS 14 DEMOS 12/8/04               
*                                                                               
         DROP  R4                                                               
         USING XDDEMBLK,R4                                                      
VVDEMNEW OI    XDOVER,X'80'        LEAVE AN OVERRIDE NOTE                       
         XC    XDDEMOS,XDDEMOS     RESET NDDEMOS BEFORE FILLING                 
         XC    XDWGTLST,XDWGTLST   NEW NDDEMOS. OLD WGTLST DOESNT APPLY         
         LA    R5,XDMAXDEM         R5 IS MAX NUMBER OF DEMOS                    
         OC    XDNDEMOS,XDNDEMOS   IF NDNDEMOS=0 USE NDMAXDEM                   
         BZ    VVDM2                                                            
         ZIC   R5,XDNDEMOS                                                      
*                                                                               
VVDM2    GOTO1 CALLOV,DMCB,0,X'D9000AD9'     DEMOVAL                            
         L     RF,DMCB                                                          
*                                                                               
         OC    NBEXTEND,NBEXTEND                                                
         JZ    VVDM4                                                            
         L     R1,NBEXTEND                                                      
         USING NBXTNDD,R1                                                       
         ICM   R6,15,NBXCDNL       DID CALLING PROGRAM SET NBXCDNL?             
         BNZ   *+8                 YES                                          
*VDM4    LA    R6,COMDLIST         NO - USE LOCAL AREA FOR COMSCORE             
VVDM4    LA    R6,ELEM             NO - USE LOCAL AREA FOR COMSCORE             
         DROP  R1                                                               
*                                                                               
         BRAS  RE,GETTOKEN         GET COMSCORE LICENSE                         
         GOTO1 (RF),DMCB,(R2),((R5),XDDEMOS),(C'S',(R3)),0,,(R6)                
         CLI   0(R6),0             ANY COMSCORE DEMOS?                          
         JE    VVDM6                                                            
         CLI   OVERLAY,X'07'       SEED REPORT?                                 
         JE    VVDM6               REQSML WAS ALREADY SET IN NEMED07            
         CLI   OVERLAY,X'1B'       SEED REPORT?                                 
         JE    VVDM6               REQSML WAS ALREADY SET IN NEMED07            
         MVI   REQSML,C'C'         SET IT FOR COMSCORE PASS 1/2 MODE            
*                                                                               
VVDM6    MVC   XDNDEMOS(1),DMCB+4  NUM DEMOS IN NDNDEMOS                        
         CLI   DMCB+4,0            NUM DEMOS. ZERO IF ERROR.                    
         BNZ   VVDXIT                                                           
         MVI   ERROR,INVDEM                                                     
         B     TRAPERR                                                          
*                                                                               
         DROP  R4                                                               
         USING NDDEMBLK,R4                                                      
VVDEMOLD OI    NDOVER,X'80'        LEAVE AN OVERRIDE NOTE                       
         XC    NDDEMOS,NDDEMOS     RESET NDDEMOS BEFORE FILLING                 
         XC    NDWGTLST,NDWGTLST   NEW NDDEMOS. OLD WGTLST DOESNT APPLY         
         LA    R5,NDMAXDEM         R5 IS MAX NUMBER OF DEMOS                    
         OC    NDNDEMOS,NDNDEMOS   IF NDNDEMOS=0 USE NDMAXDEM                   
         BZ    VVDM20                                                           
         ZIC   R5,NDNDEMOS                                                      
         SPACE 1                                                                
VVDM20   GOTO1 CALLOV,DMCB,0,X'D9000AD9'     DEMOVAL                            
         L     RF,DMCB                                                          
*                                                                               
         OC    NBEXTEND,NBEXTEND                                                
         JZ    VVDM22                                                           
         L     R1,NBEXTEND                                                      
         USING NBXTNDD,R1                                                       
         ICM   R6,15,NBXCDNL       DID CALLING PROGRAM SET NBXCDNL?             
         BNZ   *+8                 YES                                          
*VDM22   LA    R6,COMDLIST         NO - USE LOCAL AREA FOR COMSCORE             
VVDM22   LA    R6,ELEM             NO - USE LOCAL AREA FOR COMSCORE             
         DROP  R1                                                               
*                                                                               
         BRAS  RE,GETTOKEN         GET COMSCORE LICENSE                         
         GOTO1 (RF),DMCB,(R2),((R5),NDDEMOS),(C'S',(R3)),0,,(R6)                
         CLI   0(R6),0             ANY COMSCORE DEMOS?                          
         JE    VVDM24                                                           
         CLI   OVERLAY,X'07'       SEED REPORT?                                 
         JE    VVDM24              REQSML WAS ALREADY SET IN NEMED07            
         CLI   OVERLAY,X'1B'       SEED REPORT?                                 
         JE    VVDM24              REQSML WAS ALREADY SET IN NEMED07            
         MVI   REQSML,C'C'         SET IT FOR COMSCORE PASS 1/2 MODE            
*                                                                               
VVDM24   MVC   NDNDEMOS(1),DMCB+4  NUM DEMOS IN NDNDEMOS                        
         CLI   DMCB+4,0            NUM DEMOS. ZERO IF ERROR.                    
         BNZ   VVDXIT                                                           
         MVI   ERROR,INVDEM                                                     
         B     TRAPERR                                                          
         SPACE 1                                                                
VVDXIT   DS    0H                                                               
*                                                                               
         MVC   DBFILE,=C'NTI'      RESET UP DBLOCK                              
         MVI   DBSELMED,C'N'                                                    
         MVI   DBSELSRC,C'N'                                                    
         XIT1                                                                   
*                                                                               
         DROP  R3                                                               
         DROP  R4                                                               
*                                                                               
*COMDLIST DS    CL160               COMSCORE DEMO LIST (8*20)                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
XVDEMTYP NTR1  BASE=*,LABEL=*                                                   
         LM    R3,R4,0(R1)                                                      
         USING NDDEMBLK,R3                                                      
         ZIC   R6,0(R1)                                                         
         MH    R6,=H'3'            EACH DEMO CODE IS 3-BYTES                    
         LA    R6,NDDEMOS(R6)      DEMO CODE IN QUESTION                        
         B     VVDEMT7                                                          
         DROP  R3                                                               
         USING XDDEMBLK,R3                                                      
VVDEMT5  LA    R6,XDDEMOS(R6)                                                   
         DROP  R3                                                               
VVDEMT7  MVC   0(1,R4),1(R6)       DEMO MODIFIER                                
         CLI   1(R6),63            WEIGHTED DEMO                                
         BNE   VVDT2                                                            
         MVI   0(R4),C'T'          DEFAULT TO IMP                               
         TM    NBINDS6,NBI6XDEM        50 DEMOS?                                
         BO    VVDEMT8                                                          
         USING NDDEMBLK,R3                                                      
         CLI   NDWGTNAM,C'R'       WGTED RTG IF WGTNAM STARTS W/'R'             
         BNE   VVDTXIT                                                          
         B     VVDEMT9                                                          
         DROP  R3                                                               
         USING XDDEMBLK,R3                                                      
VVDEMT8  CLI   XDWGTNAM,C'R'                                                    
         BNE   VVDTXIT                                                          
*                                                                               
VVDEMT9  MVI   0(R4),C'R'                                                       
         B     VVDTXIT                                                          
         SPACE 1                                                                
VVDT2    CLI   1(R6),C'I'          IF I RETURN T                                
         BNE   VVDTXIT                                                          
         MVI   0(R4),C'T'                                                       
         SPACE 1                                                                
VVDTXIT  XIT1                                                                   
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* ROUTINE BUILDS THE REQUEST USING THE DATA THAT IS STORED IN                   
* AIO3. THE DATA WAS INITIALLY CREATED IN NENAV07, IT WAS PASSED                
* TO NEWRI00 VIA GLOBBER, STORED IN AIO3 AND PASSED TO NEWRI52.                 
*                                                                               
CHKSCRN  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     RA,ATWA                                                          
         USING TWADSEC,RA                                                       
         CLI   TWASCR,X'D8'                                                     
         BNE   CHKSCEX                                                          
         DROP  RA                                                               
*                                                                               
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         CLC   CONREC(7),=CL7'PHEADST'                                          
         BE    CHKSC20                                                          
         CLC   CONREC(7),=CL7'EHEADST'                                          
         BE    CHKSC20                                                          
         B     CHKSCEX                                                          
*                                                                               
CHKSC20  L     R3,AIO1                                                          
         A     R3,=F'2000'                                                      
         USING PHEADST,R3                                                       
* WHEN REPORT WILL RUN SOON,NOW                                                 
         MVC   CONWHEN(8),PHEDPRNT                                              
         MVI   CONWHENH+5,8                                                     
* OUTPUT TYPE IF E-REPORT OUTPUT WILL BE 'DOWN'                                 
         CLI   CONREC,C'E'                                                      
         BNE   CHKSC50                                                          
         MVC   CONOUT(4),=CL4'DOWN'                                             
         MVI   CONOUTH+5,4                                                      
********** DESTINATION                                                          
***********CHKSC40  CLI   PHEDDEST,X'40'                                        
***********         BNH   CHKSC50                                               
***********         MVC   CONDEST(8),PHEDDEST                                   
***********         MVI   CONDESTH+5,8                                          
* DUMMY KEY                                                                     
CHKSC50  MVC   SPLNAM(3),=CL3'EDI'                                              
         MVI   SPLNAMH+5,3                                                      
* CLIENT                                                                        
CHKSC60  MVC   SPLCLI(8),PHEDCLT                                                
         MVI   SPLCLIH+5,8                                                      
* ESTIMATE                                                                      
         MVC   SPLEST(8),PHEDEST                                                
         MVI   SPLESTH+5,8                                                      
* NETWORK                                                                       
         MVC   SPLNET(8),PHEDNET                                                
         MVI   SPLNETH+5,8                                                      
* PACKAGE                                                                       
         MVC   SPLPAK(8),PHEDPKG                                                
         MVI   SPLPAKH+5,8                                                      
* DEMOS                                                                         
         CLI   PHEDDEMS,X'40'                                                   
         BNH   CHKSC100                                                         
         MVC   SPLDEM(64),PHEDDEMS                                              
         MVI   SPLDEMH+5,40                                                     
* COMMENT 1 ONLY ONE WITH EXTENDED HEADER                                       
CHKSC100 LA    RE,SPLCOMH                                                       
         MVI   5(RE),0                                                          
         CLC   PHEDCOM1,=50X'40'                                                
         BE    CHKSC120                                                         
         MVC   SPLCOM,PHEDCOM1                                                  
         MVI   5(RE),50                                                         
CHKSC120 ZIC   RF,0(RE)             GET NEXT COMMENT LINE                       
         AR    RE,RF                                                            
         MVI   5(RE),0                                                          
         CLC   PHEDCOM2,=50X'40'                                                
         BE    CHKSC140                                                         
         MVC   8(50,RE),PHEDCOM2                                                
         MVI   5(RE),50                                                         
CHKSC140 ZIC   RF,0(RE)             GET NEXT COMMENT LINE                       
         AR    RE,RF                                                            
         MVI   5(RE),0                                                          
         CLC   PHEDCOM3,=50X'40'                                                
         BE    CHKSC160                                                         
         MVC   8(50,RE),PHEDCOM3                                                
         MVI   5(RE),50                                                         
CHKSC160 ZIC   RF,0(RE)             GET NEXT COMMENT LINE                       
         AR    RE,RF                                                            
         MVI   5(RE),0                                                          
         CLC   PHEDCOM4,=50X'40'                                                
         BE    CHKSC180                                                         
         MVC   8(50,RE),PHEDCOM4                                                
         MVI   5(RE),50                                                         
CHKSC180 ZIC   RF,0(RE)             GET NEXT COMMENT LINE                       
         AR    RE,RF                                                            
         MVI   5(RE),0                                                          
         CLC   PHEDCOM5,=50X'40'                                                
         BE    CHKSC200                                                         
         MVC   8(50,RE),PHEDCOM5                                                
         MVI   5(RE),50                                                         
CHKSC200 ZIC   RF,0(RE)             GET NEXT COMMENT LINE                       
         AR    RE,RF                                                            
         MVI   5(RE),0                                                          
         CLC   PHEDCOM6,=50X'40'                                                
         BE    CHKSC220                                                         
         MVC   8(50,RE),PHEDCOM6                                                
         MVI   5(RE),50                                                         
* DEAL                                                                          
CHKSC220 CLI   PHEDDEAL,X'40'                                                   
         BNH   CHKSC300                                                         
         MVC   SPLDEL(10),PHEDDEAL                                              
         MVI   SPLDELH+5,10                                                     
* CONTRACT                                                                      
CHKSC300 CLI   PHEDCONT,X'40'                                                   
         BNH   CHKSCEX                                                          
         MVC   SPLCON(10),PHEDCONT                                              
         MVI   SPLCONH+5,10                                                     
*                                                                               
CHKSCEX  XIT1                                                                   
         DROP  R2,R3                                                            
         LTORG                                                                  
*              NEGENINCLS                                                       
*              NETDEMOD                                                         
*              NETUNIVD                                                         
*              DEDBLOCK                                                         
*              SPGENAGY                                                         
*              SPGENCLT                                                         
*              SPGENPRD                                                         
*              SPGENEST                                                         
*              SPGENSTA                                                         
*              DDCOMFACS                                                        
*              NEGETNUND                                                        
*              DRGLOBAL                                                         
*              DDGENCTWAD                                                       
*              NEGENNBUFF                                                       
*              NEGENTBUFF                                                       
*              FATIOB                                                           
*              DDOFFICED                                                        
*              CTGENFILE                                                        
         SPACE 1                                                                
         PRINT OFF                                                              
       ++INCLUDE NEGENINCLN                                                     
         PRINT ON                                                               
**************** NETDEMOD                                                       
       ++INCLUDE NETDEMOT                                                       
       ++INCLUDE NETUNIVD                                                       
       ++INCLUDE DEDBLOCK                                                       
         PRINT OFF                                                              
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE SPGENAGY                                                       
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE SPGENPRG                                                       
       ++INCLUDE SPGENGRP                                                       
       ++INCLUDE SPGENCLG                                                       
       ++INCLUDE SPGENREAS                                                      
       ++INCLUDE CTGENDIC                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FAXTRAINF                                                      
       ++INCLUDE NEGETNUND                                                      
       ++INCLUDE DRGLOBAL                                                       
       ++INCLUDE SEACSFILE                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE NETDEMON                                                       
TWADSEC  DSECT                                                                  
       ++INCLUDE DDGENCTWAD                                                     
       ++INCLUDE NEGENNBUFF                                                     
       ++INCLUDE NEGENTBUFF                                                     
       ++INCLUDE FATIOB                                                         
       ++INCLUDE DDOFFICED                                                      
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE FASECRETD                                                      
       ++INCLUDE NETSECD                                                        
       ++INCLUDE DDMASTC                                                        
         PRINT ON                                                               
       ++INCLUDE NEGENDPT                                                       
       ++INCLUDE NAVDSECTS                                                      
         PRINT ON                                                               
       ++INCLUDE NEWRIFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRID8D                                                       
       ++INCLUDE NETBLKXTND                                                     
       ++INCLUDE GEGENTOK                                                       
       ++INCLUDE DEDEMOVALD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010NEMEDGEN  11/09/18'                                      
         END                                                                    
