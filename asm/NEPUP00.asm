*          DATA SET NEPUP00    AT LEVEL 227 AS OF 05/01/02                      
*PHASE T32200A,*                                                                
*INCLUDE KHDUMMY                                                                
         TITLE 'T32200 - UPFRONT PLANNING CONTROLLER'                           
T32200   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LENWORK,T32200**,RA,RR=R2,CLEAR=YES                              
         USING GEND,RC                                                          
         SPACE 1                                                                
         USING SPOOLD,R8                                                        
         USING SYSD,R9             SYSTEM SPECIFIC WORK                         
         SPACE 1                                                                
         ST    R2,RELO                                                          
         LR    R8,RC               R8=A(SPOOLD)                                 
         LA    RC,SPOOLEND         RC=A(GENCON STORAGE)                         
         LA    R9,IO               START OF IO 1 + TOTAL IO AREA LEN            
         AH    R9,=Y(IOLEN)        R9=A(PUP SYSTEM WORKING STORAGE)             
         SPACE 1                                                                
         ST    R1,SYSPARMS         THIS CODE FOR CURSOR POSITIONING             
         L     RF,0(R1)                                                         
         ST    RF,ATIOB                                                         
*                                                                               
         USING TIOBD,RF                                                         
         ZIC   R0,TIOBAID          NUMBER OF PFKEY PRESSED                      
         C     R0,=F'12'                                                        
         BNH   *+8                                                              
         S     R0,=F'12'                                                        
         STC   R0,PFKEY            PFKEY ADJUSTED TO 1..12 (0 = ENTER)          
         DROP  RF                                                               
*                                                                               
         L     R7,4(R1)                                                         
         ST    R7,ATWA             R7=A(TWA)                                    
         USING CONHEADH-64,R7                                                   
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(131),SPACES                                             
         SPACE 1                                                                
         ST    RB,SYSRB            SYSTEM BASE REGS                             
         ST    RA,SYSRA                                                         
         BAS   RE,SYSINIT          INITIALIZE SYSTEM DEPENDENT                  
         SPACE 1                                                                
         BAS   RE,SETRD            SET RD SO GENCON WILL RETURN                 
         SPACE 1                                                                
         XC    DMCB,DMCB           OBTAIN CORE-RESIDENT ADDRESSES               
         LA    R2,CORETAB                                                       
         LA    R0,CORES            COUNTER                                      
         LA    R3,COREFACS         POINT TO ADDRESS AREA                        
         L     RF,CALLOV                                                        
         LA    R1,DMCB                                                          
         MVC   DMCB+4(3),=X'D9000A'                                             
         SPACE 1                                                                
RES10    MVC   DMCB+7(1),0(R2)                                                  
         GOTO1 (RF),(R1),0                                                      
         MVC   0(4,R3),DMCB        SAVE MODULE ADDRESS                          
         LA    R2,1(R2)            NEXT MODULE NUMBER                           
         LA    R3,4(R3)            NEXT ADDRESS                                 
         BCT   R0,RES10                                                         
         SPACE 1                                                                
         GOTO1 GENCON,DMCB,(R8)     CALL GENCON                                 
         SPACE 1                                                                
EXIT     XIT1                                                                   
         SPACE 2                                                                
SETRD    NTR1                                                                   
         ST    RD,SYSRD            SET RD SO GENCON WILL RETURN CONTROL         
         B     EXIT                                                             
         EJECT                                                                  
*              INITIALIZE FOR SYSTEM                                            
         SPACE 3                                                                
SYSINIT  NTR1                                                                   
         SPACE 1                   GET TERMINAL VALUES                          
         MVI   DDS,C'N'                                                         
         CLI   TWAOFFC,C'*'        TEST FOR DDS TERMINAL                        
         BNE   *+8                                                              
         MVI   DDS,C'Y'                                                         
         MVC   TERM,TWATRM                                                      
         MVC   AUTH,TWAAUTH                                                     
         MVC   USERID,TWAORIG                                                   
         MVC   AGENCY,TWAAGY                                                    
         MVI   TWANSAVE,0          OUTSMART GENCON - DON'T RESTORE              
         SPACE 1                                                                
SYS1     LA    R2,SYSV                                                          
         LA    R3,SYSVCON                                                       
         LA    R4,NVTYPES                                                       
         SPACE 1                                                                
SYS2     L     R1,0(R3)            RELOCATE SYSTEM VTYPES                       
         A     R1,RELO                                                          
         ST    R1,0(R2)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,SYS2                                                          
         SPACE 1                                                                
         LA    R2,VCOMMON          SET UP COMMON ENTRIES                        
         SR    R3,R3                                                            
         LA    R4,SYSCOMM                                                       
         LA    R5,VCOUNT                                                        
         SPACE 1                                                                
SYS4     ST    R2,0(R4)                                                         
         STC   R3,0(R4)                                                         
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   R5,SYS4                                                          
         SPACE 1                                                                
*                                  SEED DSECT FOR DUMP READABILITY              
         MVC   DUMPSYSD,=C'**SYSD**'                                            
         MVC   DUMPCOMM,=C'*COMMON*'                                            
         MVC   DUMPDB,=C'*DBLOCK*'                                              
         MVC   DUMPHUT,=C'**HUT***'                                             
         MVC   DUMPEVN,=C'**EVN***'                                             
         MVC   DUMPBUFF,=C'**BUFF**'                                            
         MVC   DUMPHEAD,=C'**HEAD**'                                            
         MVC   DUMPPLAN,=C'**PLAN**'                                            
         MVC   DUMPPROG,=C'**PROG**'                                            
         MVC   DUMPDEMS,=C'**DEMS**'                                            
         MVC   DUMPFCIL,=C'*FACILS*'                                            
         MVC   DUMPWORK,=C'**WORK**'                                            
         EJECT                                                                  
*              SET SYSTEM DEPENDENT VALUES                                      
         SPACE 3                                                                
         MVI   SYSTEM,C'N'         NETWORK                                      
         MVI   GETMSYS,5           GENCON MESSAGES                              
         SPACE 1                                                                
         MVI   MAXIOS,NIOS         USES 2 I/O AREAS                             
         MVC   SIZEIO,=AL4(LIOS)   EACH I/O IS 2000 BYTES                       
         MVC   SYSDUMMY,VDUMMY     END OF SYSTEM BASE                           
         MVC   GETUSER,VGETAGY     ROUTINE TO GET AGY NAME AND ADDRESS          
         SPACE 1                                                                
*                                                                               
         MVC   LKEY,=H'20'          DETAILS OF DIRECTORY AND KEY                
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'27'                                                  
         MVC   SYSFIL,=C'UNTFIL  '                                              
         MVC   SYSDIR,=C'UNTDIR  '                                              
         MVC   REQFILE,=C'REQUEST '                                             
         SPACE 1                                                                
SYS8     MVC   LWORK,=Y(LENWORK)        SET WORK AREA LENGTH                    
         MVC   RCPROG(2),=C'NE'         PREFIX FOR REPORT NO.                   
         MVC   SYSPHASE,=X'D9032200'    PRESET FOR SYSTEM CALLOVS               
         L     R1,=A(RECACTS)           RECORD/ACTION DIRECTORY                 
         A     R1,RELO                                                          
         ST    R1,ARECACT                                                       
         SPACE 1                                                                
*                                  SET UP CERTAIN ADDRESSES                     
         L     R1,SYSPARMS                                                      
         LM    R2,R4,8(R1)         A(SYSLIST) A(TIA) A(COMFACS)                 
         ST    R4,ACOMFACS                                                      
         USING COMFACSD,R4                                                      
         ST    R3,ATIA                                                          
         MVC   CALLOV,CCALLOV                                                   
         MVC   DATAMGR,CDATAMGR                                                 
         MVC   GETMSG,CGETMSG                                                   
         MVC   HELLO,CHELLO                                                     
         MVC   SCANNER,CSCANNER                                                 
         MVC   DEMOUT,CDEMOUT                                                   
         MVC   DEMOVAL,CDEMOVAL                                                 
         MVC   DEMAND,CDEMAND                                                   
         B     XIT                                                              
         DROP  R4                                                               
         SPACE 2                                                                
IOLEN    EQU   NIOS*(LIOS+8)                                                    
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              SYSTEM ROUTINES ENTERABLE FROM BASE OF OVERLAY                   
         SPACE 3                                                                
         DS    0H                                                               
VCOMMON  NTR1  BASE=SYSRB                                                       
         L     R7,ATWA                                                          
         L     R8,ASPOOLD                                                       
         L     R9,ASYSD            RESTORE POINTER TO SYSTEM STORAGE            
         L     RA,SYSRA            2ND BASE REG - R9 MUST BE SET FIRST          
         ST    RD,COMMRD                                                        
         SRL   RF,24                                                            
         B     VBRANCH(RF)                                                      
         SPACE 1                                                                
VBRANCH  B     GETAGY                                                           
         B     VALCLT                                                           
         B     VALNET                                                           
         B     VALDPT                                                           
         B     VALPLAN                                                          
         B     EXTPLAN                                                          
         B     EXTBUDG                                                          
         B     LUPUNIV                                                          
         B     VALPROG                                                          
         B     EXTPROG                                                          
         B     LUPPROG                                                          
         B     LUPHUT                                                           
         B     EXTDEM                                                           
         B     EXTUNS                                                           
         B     VALDEM                                                           
         B     GETDEM                                                           
         B     LUPDPT                                                           
         B     VPUPIO                                                           
         B     LUPCPRG                                                          
         B     SPARE                                                            
         B     SPARE                                                            
         B     SPARE                                                            
         B     VALTITL                                                          
         B     TITOUT                                                           
         B     UPHEAD                                                           
         B     VALFILT                                                          
         B     CHEFILT                                                          
         B     ERRXIT                                                           
         B     CURSERR                                                          
         B     VVSETUNT                                                         
         B     VVSETSPT                                                         
         B     SETDB                                                            
         SPACE 1                                                                
VCOUNT   EQU   (*-VBRANCH)/4                                                    
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              GET AGENCY DATA FORM CONTROL FILE                                
         SPACE 3                                                                
*                                  AGENCY NAME & ADDRESS FROM ID REC.           
GETAGY   MVC   FILENAME,=CL8'CTFILE'                                            
         MVI   USEIO,C'Y'                                                       
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTIKEY,R4                                                        
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),USERID                                               
         GOTO1 READ                                                             
         GOTO1 HELLO,PARAS,(C'G',FILENAME),(X'36',AIO),0                        
         L     R6,12(R1)                                                        
         USING CTORGD,R6                                                        
         MVC   USERNAME,CTORGNAM                                                
         MVC   USERADDR,CTORGADD                                                
         XC    FILENAME,FILENAME                                                
         MVI   USEIO,0                                                          
         SPACE 1                                                                
         GOTO1 VSETSPOT                                                         
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING AGYKEY,R4                                                        
         MVI   AGYKTYPE,X'06'                                                   
         MVC   AGYKAGY,AGENCY                                                   
         GOTO1 READ                                                             
         MVC   FILENAME,=CL8'SPTFILE'                                           
         GOTO1 GETREC                                                           
         LA    R6,IO                                                            
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         B     GETAGY6                                                          
         SPACE 1                                                                
GETAGY4  BAS   RE,NEXTEL                                                        
         SPACE 1                                                                
GETAGY6  BNE   GETAGYX                                                          
         CLI   2(R6),C'N'          LOOK FOR NETWORK ELEMENT                     
         BNE   GETAGY4                                                          
         MVC   BINAGYMD,3(R6)      DIG OUT AGENCY/MEDIA CODE                    
         SPACE 1                                                                
GETAGYX  GOTO1 VSETUNT                                                          
         BAS   RE,INITSEC                                                       
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
* INITIALIZE SECRET                                                             
*                                                                               
INITSEC  NTR1                                                                   
         L     RE,ATWA                                                          
         A     RE,=A(BASETWA)                                                   
         USING SAVAREA,RE                                                       
*                                                                               
         SPACE 1                                                                
         OC    TWASAGN,TWASAGN          ON NEW SECURITY                         
         BNZ   *+14                                                             
         OC    TWAACCS(2),TWAACCS       OR HAVE LIMIT ACCESS                    
         BZ    INITSCEX                                                         
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSECRET-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,('SECPINIT',SVSECRET),0                                
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
INITSCEX B     XIT                                                              
         DROP  RE                                                               
         EJECT                                                                  
*              ROUTINE TO VALIDATE CLIENT                                       
         SPACE 3                                                                
VALCLT   GOTO1 ANY                                                              
         MVC   CLTCODE,WORK                                                     
         GOTO1 CLPACK,DMCB,CLTCODE,CLTCOMP                                      
         GOTO1 VSETSPOT                                                         
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CKEY,R4                                                          
         MVC   CKEYAM,BINAGYMD                                                  
         MVC   CKEYCLT,CLTCOMP                                                  
         GOTO1 READ                                                             
         MVC   FILENAME,=CL8'SPTFILE'                                           
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         MVC   CLTNAME,CNAME       CLIENT DETAILS                               
         MVC   CLTOFF,COFFICE                                                   
         SPACE 1                                                                
         XC    KEY,KEY             HANDLE THE PROFILES                          
         MVC   KEY(4),=C'S0N0'                                                  
         MVC   KEY+4(2),AGENCY                                                  
         MVI   KEY+6,C'N'                                                       
         MVC   KEY+7(3),CLTCODE                                                 
         MVI   KEY+10,C'*'                                                      
         MVC   KEY+11(1),CLTOFF                                                 
         GOTO1 GETPROF,DMCB,KEY,N0PROF,DATAMGR      N0                          
         MVI   KEY+3,C'1'                                                       
         GOTO1 (RF),(R1),,N1PROF                    N1                          
         MVI   KEY+3,C'2'                                                       
         GOTO1 (RF),(R1),,N2PROF                    N2                          
         MVC   KEY+2(2),=C'00'                                                  
         GOTO1 (RF),(R1),,ZEROPROF                  00                          
         GOTO1 VSETUNT                                                          
         OI    6(R2),X'80'                                                      
         BAS   RE,CALLOFCR                                                      
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*=========================================================                      
* CALL OFFICER TO VALIDATE LIMIT ACCESS                                         
*=========================================================                      
         SPACE 1                                                                
CALLOFCR NTR1                                                                   
         L     R4,AIO                                                           
         USING CKEY,R4                                                          
*                                                                               
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A38' GET OFFICER ADDRESS                       
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    WORK,WORK                                                        
         LA    R1,WORK                                                          
         USING OFFICED,R1                                                       
*                                                                               
         L     RE,ATWA                                                          
         A     RE,=A(BASETWA)                                                   
         USING SAVAREA,RE                                                       
*                                                                               
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAUTH,TWAACCS                                                  
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC,COFFICE                                                   
         MVC   OFCCLT,CLTCODE                                                   
         OC    OFCCLT,SPACES                                                    
         MVC   OFCSAGMD,BINAGYMD                                                
         MVC   OFCLMT(4),TWAACCS                                                
         MVC   OFCACCSC(3),CACCESS    ACCESS LIST FROM CLTHDR                   
         LA    RF,SVSECRET                                                      
         ST    RF,OFCSECD                                                       
         DROP  R1,RE                                                            
*                                                                               
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(C'N',WORK),ACOMFACS                                   
         CLI   0(R1),0                                                          
         BE    CALLOFEX                                                         
*                                                                               
         MVC   CONHEAD(L'INVNET),SECERR    SECURITY ERROR                       
         B     MYEND                                                            
*                                                                               
CALLOFEX B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              VALIDATE NETWORK                                                 
         SPACE 3                                                                
VALNET   CLI   5(R2),0                                                          
         BE    VN2                                                              
         MVC   NETWORK,8(R2)                                                    
         CLI   NETWORK+3,X'40'                                                  
         BH    *+8                                                              
         MVI   NETWORK+3,X'40'                                                  
         CLC   =C'ALL',NETWORK                                                  
         BNE   VN3                                                              
VN2      TM    ALLOKS,X'04'                                                     
         BZ    BADALL                                                           
         XC    NETWORK,NETWORK                                                  
         B     XIT                                                              
VN3      MVC   KEY(17),=C'SNABC NAA00000000'                                    
         USING STAREC,R4                                                        
         LA    R4,KEY                                                           
         MVC   STAKCALL(4),NETWORK                                              
         MVC   STAKAGY,AGENCY                                                   
         MVI   USEIO,C'Y'                                                       
         MVC   FILENAME,=C'STATION'                                             
         GOTO1 READ                                                             
         L     R4,AIO                                                           
         MVC   NETNTIST,SNTISTA    SAVE NTI STATION                             
         PACK  DUB,SMKT            CONVERT 'MARKET' NUMBER                      
         CVB   R1,DUB                                                           
         STH   R1,DUB                                                           
         MVC   NETMKTN,DUB                                                      
         MVC   NETMEDIA,STYPE                                                   
         MVI   USEIO,C'N'                                                       
         XC    FILENAME,FILENAME                                                
         OI    6(R2),X'80'                                                      
         MVI   NETMAIN,C'Y'                                                     
         CLC   NETWORK(3),=C'ABC'                                               
         BE    XIT                                                              
         CLC   NETWORK(3),=C'CBS'                                               
         BE    XIT                                                              
         CLC   NETWORK(3),=C'NBC'                                               
         BE    XIT                                                              
         CLI   STYPE,C'S'          LEAVE IT ALONE FOR SYND                      
         BE    XIT                                                              
         CLI   STYPE,C'N'          LEAVE IT ALONE FOR NET PXZ                   
         BE    XIT                                                              
         MVI   NETMAIN,C'N'                                                     
         B     XIT                                                              
         SPACE 1                                                                
BADNET   MVC   CONHEAD(L'INVNET),INVNET    INVALID NETWORK                      
         B     MYEND                                                            
BADALL   MVC   CONHEAD(L'INVALL),INVALL ALL NOT ALLOWED                         
         B     MYEND                                                            
         DROP  R4                                                               
         EJECT                                                                  
*              DAYPART ROUTINES                                                 
         SPACE 3                                                                
VALDPT   LA    R1,DAYPLKUP         VALIDATE DAYPART                             
         ZIC   R3,5(R2)                                                         
         BCTR  R3,0                                                             
         CLI   5(R2),0                                                          
         BNE   VALDPT2                                                          
         TM    ALLOKS,X'02'                                                     
         BZ    BADDPT                                                           
         MVI   DPTCODE,0                                                        
         B     XIT                                                              
         SPACE 1                                                                
VALDPT2  MVC   DPTCODE,0(R1)                                                    
         MVC   DPTNAME,1(R1)                                                    
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),1(R1)       CHECK AGAINST DP NAME                        
         BE    XIT                                                              
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),0(R1)       THEN AGAINST DP CODE                         
         BE    XIT                                                              
         LA    R1,9(R1)                                                         
         CLI   0(R1),X'FF'                                                      
         BNE   VALDPT2                                                          
         SPACE 1                                                                
BADDPT   MVC   CONHEAD(L'INVDPT),INVDPT    INVALID DAYPART                      
         B     MYEND                                                            
         SPACE 1                                                                
LUPDPT   LA    R1,DAYPLKUP         LOOK UP DAYPART                              
         SPACE 1                                                                
LUPDPT2  MVC   DPTCODE,0(R1)                                                    
         MVC   DPTNAME,1(R1)                                                    
         CLC   WORK(1),0(R1)                                                    
         BE    XIT                                                              
         CLI   0(R1),X'FF'                                                      
         BE    XIT                                                              
         LA    R1,9(R1)                                                         
         B     LUPDPT2                                                          
         SPACE 1                                                                
DAYPLKUP DS    0H                                                               
DAYPLEN1 EQU   1                   LENGTH OF KEY VALUE                          
DAYPLEN2 EQU   8                   LENGTH OF RETURNED VALUE                     
         DC    CL1'D',CL8'DAYTIME'                                              
         DC    CL1'F',CL8'FRINGE'                                               
         DC    CL1'P',CL8'PRIME'                                                
         DC    CL1'K',CL8'KIDS'                                                 
         DC    CL1'S',CL8'SPORTS'                                               
         DC    CL1'N',CL8'NEWS'                                                 
         DC    CL1'L',CL8'LATE'                                                 
         DC    CL1'Y',CL8'YOUTH'                                                
         DC    CL1'E',CL8'EARLY'                                                
         DC    CL1'T',CL8'TEENS'                                                
         DC    CL1'C',CL8'CABLE'                                                
         DC    CL1'X',CL8'SYND'                                                 
         DC    CL1'I',CL8'SPECIAL'                                              
         DC    CL1'O',CL8'OLYMPICS'                                             
         DC    CL1'R',CL8'RADIO'                                                
         DC    CL1'V',CL8'OVRNGHT'                                              
         DC    CL1'W',CL8'WKND EVE'                                             
         DC    CL1'M',CL8'WKND MRN'                                             
         DC    CL1'A',CL8'ACCESS'                                               
         DC    CL1'U',CL8'UNWIRED'                                              
         DC    CL1'B',CL8'CBLSPORT'                                             
         DC    CL1'Q',CL8'INTRACTV'                                             
         DC    CL1'H',CL8'OTHER'                                                
         DC    CL1'J',CL8'PROMO-ID'                                             
         DC    XL1'FF',CL8' '      END OF TABLE                                 
         EJECT                                                                  
*              I/O FOR PLAN/PROG RECORDS                                        
         SPACE 3                                                                
VPUPIO   DS    0H                                                               
         MVC   APUPHOOK,0(R1)      STORE USER PUPIO HOOK ADDRESS                
         MVC   USERRD,4(RD)        SAVE USER RD CHAIN                           
* RE-READ PLAN RECORD                                                           
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING NPLKEY,R4                                                        
         MVI   NPLKTYPE,X'20'      FILL PLAN KEY                                
         MVC   NPLKAM,BINAGYMD                                                  
         MVC   NPLKCLT,CLTCOMP                                                  
         MVC   NPLKNET,NETWORK                                                  
         MVC   NPLKDPT,DPTCODE                                                  
         MVC   NPLKPLAN,PLANCODE                                                
PIO2     GOTO1 HIGH                                                             
         B     PIO3B                                                            
         SPACE 1                                                                
PIO3     GOTO1 SEQ                                                              
         SPACE 1                                                                
PIO3B    CLC   KEY(4),KEYSAVE      AGY/CLT                                      
         BNE   PIOX                                                             
         LA    R4,KEY                                                           
         CLI   NETWORK,0                                                        
         BE    *+14                                                             
         CLC   NPLKNET,KEYSAVE+5                                                
         BNE   PIO3                                                             
         CLI   DPTCODE,0                                                        
         BE    *+14                                                             
         CLC   NPLKDPT,KEYSAVE+10                                               
         BNE   PIO3                                                             
         OC    PLANCODE,PLANCODE                                                
         BZ    *+14                                                             
         CLC   NPLKPLAN,KEYSAVE+11                                              
         BNE   PIO3                                                             
PIO5     GOTO1 GETREC                                                           
         CLI   NETWORK,0           IF MEDIA=ALL                                 
         BNE   *+8                                                              
         BAS   RE,GETSTAT          READ STATION REC FOR MEDIA TYPE              
         MVI   NETMAIN,C'Y'                                                     
         CLC   NPLKNET(3),=C'ABC'                                               
         BE    PIO5B                                                            
         CLC   NPLKNET(3),=C'CBS'                                               
         BE    PIO5B                                                            
         CLC   NPLKNET(3),=C'NBC'                                               
         BE    PIO5B                                                            
         CLI   NETMEDIA,C'S'          LEAVE IT ALONE FOR SYND                   
         BE    PIO5B                                                            
         CLI   NETMEDIA,C'N'          LEAVE IT ALONE FOR NET PXZ                
         BE    PIO5B                                                            
         MVI   NETMAIN,C'N'                                                     
PIO5B    GOTO1 VEXTPLAN                                                         
         BAS   RE,CHKFILT                                                       
         BNE   PIO3                                                             
         MVC   PLANKEYS,KEY        SAVE CURRENT PLAN KEY                        
         MVI   PUPMODE,PLANFRST                                                 
         BAS   RE,GOHOOK                                                        
         CLI   PUPMODE,PUPIOEND                                                 
         BE    PIOX                                                             
*    READ PROGRAM RECORDS                                                       
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING NPUKEY,R4           FILL PROGRAM KEY                             
         LA    R5,PLANKEYS         FROM PLANKEYS                                
         USING NPLKEY,R5                                                        
         MVI   NPUKTYPE,X'22'                                                   
         MVC   NPUKAM,NPLKAM                                                    
         MVC   NPUKCLT,NPLKCLT                                                  
         MVC   NPUKNET,NPLKNET                                                  
         MVC   NPUKDPT,NPLKDPT                                                  
         MVC   NPUKPLAN,NPLKPLAN                                                
         GOTO1 HIGH                                                             
         B     PIO6                                                             
         SPACE 1                                                                
PIO4     GOTO1 SEQ                                                              
         SPACE 1                                                                
PIO6     CLC   KEY(13),KEYSAVE     MUST MATCH ON PLAN                           
         BNE   PIO10                                                            
         CLI   PUPQFLG,C'Y'        ARE WE FILTERING QUARTERS                    
         BNE   PIO7                                                             
         CLC   PUPQFLT,NPUKPERQ                                                 
         BNE   PIO4                                                             
PIO7     GOTO1 GETREC                                                           
         GOTO1 VEXTPROG                                                         
         MVI   PUPMODE,PROGMD                                                   
         BAS   RE,GOHOOK                                                        
         B     PIO4                                                             
         SPACE 1                                                                
PIO10    MVI   PUPMODE,PLANLST     HOOK TO USER WITH EOF FOR PLAN               
         BAS   RE,GOHOOK                                                        
         XC    KEY,KEY                                                          
         MVC   KEY(20),PLANKEYS                                                 
         GOTO1 HIGH                                                             
         B     PIO3                                                             
         SPACE 1                                                                
PIOX     B     XIT                                                              
         DROP  R4,R5                                                            
         EJECT                                                                  
*        ROUTINE TO CHECK KEY AGAINST PLAN FILTER                               
         SPACE 3                                                                
*                                                                               
*                                                                               
CHKFILT  NTR1                                                                   
         OC    PUPPFLT,PUPPFLT       ARE WE FILTERING ON PLAN                   
         BZ    CHKYES                NO / SO EXIT WITH YES                      
         LA    R3,PUPPFLT                                                       
         LA    R5,PLANFILT                                                      
         LA    R0,3                                                             
         SPACE 1                                                                
CHK4     CLI   0(R3),C'*'          WILD                                         
         BE    CHK8                                                             
         CLI   0(R3),0                                                          
         BE    CHK8                                                             
         TM    0(R3),X'40'                                                      
         BZ    CHK7                                                             
         CLC   0(1,R3),0(R5)       FILTER                                       
         BNE   CHKNO                                                            
         B     CHK8                                                             
         SPACE 1                                                                
CHK7     MVC   BYTE,0(R5)                                                       
         NI    BYTE,X'FF'-X'40'     TURN OFF X'40' BIT                          
         CLC   0(1,R3),BYTE         NEGATIVE FILTER                             
         BE    CHKNO                                                            
         SPACE 1                                                                
CHK8     LA    R3,1(R3)                                                         
         LA    R5,1(R5)                                                         
         BCT   R0,CHK4                                                          
CHKYES   SR    R1,R1               PASSED FILTERS                               
         B     *+8                                                              
CHKNO    LA    R1,1                                                             
         LTR   R1,R1                                                            
         B     XIT                                                              
         EJECT                                                                  
*        HOOK BACK TO USER WITH PLAN/PROG RECORDS                               
         SPACE 3                                                                
GOHOOK   NTR1                                                                   
         L     RF,APUPHOOK         USERS HOOK ADDRESS                           
         L     RE,USERRD                                                        
         LM    R0,RC,20(RE)        RESTORE USERS R0-RC                          
         BASR  RE,RF                                                            
         XIT1                                                                   
         EJECT                                                                  
*              ROUTINE TO VALIDATE PLAN                                         
         SPACE 3                                                                
VALPLAN  CLI   5(R2),0             IS IT ALL                                    
         BE    VPL0                                                             
         CLC   =C'ALL',8(R2)                                                    
         BNE   VPL1                                                             
VPL0     TM    ALLOKS,X'01'                                                     
         BZ    BADALL                                                           
         XC    PLANCODE,PLANCODE                                                
         B     VPL2B                                                            
*                                                                               
VPL1     CLC   =C'F=',8(R2)        OR FILTER                                    
         BNE   VPL2                                                             
         MVC   PUPPFLT,10(R2)                                                   
         XC    PLANCODE,PLANCODE                                                
         B     VPL2B                                                            
*                                                                               
VPL2     MVC   PLANCODE,8(R2)      A CODE                                       
         LA    R1,4                                                             
         LA    R3,PLANCODE                                                      
VPLOOP   CLI   0(R3),0             SET END ZEROS TO BLANKS                      
         BNE   *+8                                                              
         MVI   0(R3),X'40'                                                      
         LA    R3,1(R3)                                                         
         BCT   R1,VPLOOP                                                        
VPL2B    LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING NPLKEY,R4                                                        
         MVI   NPLKTYPE,X'20'      FILL PLAN KEY                                
         MVC   NPLKAM,BINAGYMD                                                  
         MVC   NPLKCLT,CLTCOMP                                                  
         MVC   NPLKNET,NETWORK                                                  
         MVC   NPLKDPT,DPTCODE                                                  
         MVC   NPLKPLAN,PLANCODE                                                
         GOTO1 HIGH                                                             
         B     VPL3B                                                            
         SPACE 1                                                                
VPL3     GOTO1 SEQ                                                              
         SPACE 1                                                                
VPL3B    CLC   KEY(4),KEYSAVE      AGY/MED/CLT                                  
         BNE   BADPLAN                                                          
         CLI   NETWORK,0                                                        
         BE    *+14                                                             
         CLC   NPLKNET,KEYSAVE+5                                                
         BNE   VPL3                                                             
         CLI   DPTCODE,0                                                        
         BE    *+14                                                             
         CLC   NPLKDPT,KEYSAVE+10                                               
         BNE   VPL3                                                             
         OC    PLANCODE,PLANCODE                                                
         BZ    *+14                                                             
         CLC   NPLKPLAN,KEYSAVE+11                                              
         BNE   VPL3                                                             
         GOTO1 GETREC                                                           
         GOTO1 VEXTPLAN                                                         
         BAS   RE,CHKFILT                                                       
         BNE   VPL3                                                             
         OI    6(R2),X'80'                                                      
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
EXTPLAN  L     R4,AIO              VALUES FROM MAIN ELEMENT                     
         USING NPLRECD,R4                                                       
         MVC   PLANYEAR,NPLNYEAR                                                
         MVC   PLANNAME,NPLNNAME                                                
         MVC   PLANUNIV,NPLNUNIV                                                
         MVC   PLANNLEN,NPLNNLEN                                                
         MVC   PLANLENS,NPLNLENS                                                
         MVC   PLANHTYR,NPLNHTYR                                                
         MVC   PLANHTNO,NPLNHTNO                                                
         MVC   PLANHTSC,NPLNHTSC                                                
         MVC   PLANHTAV,NPLNHAVE                                                
         MVC   PLANHTYP,NPLNHTBT                                                
         MVC   PLANHTPO,NPLNHTPO                                                
         MVC   PLANHTFL,NPLNHTFL                                                
         MVC   PLANPERT,NPLNPERT                                                
         CLI   PLANPERT,0                                                       
         BNE   *+10                                                             
         MVC   PLANPERT,PLANHTAV                                                
         MVI   PLANPRCB,C'N'                                                    
         TM    NPLNOPTS,X'80'      CABLE PROGRAM                                
         BZ    *+8                                                              
         MVI   PLANPRCB,C'Y'                                                    
         MVC   PLANPRFL,NPLNPRFL                                                
         CLI   PLANPRFL,0          OVERRIDE N0 PROFILE IF SET                   
         BE    *+10                                                             
         MVC   N0PROF+3(1),PLANPRFL                                             
         MVC   PLANFILT,NPLNFILT                                                
         MVC   NDEMOS,NPLNNDEM                                                  
         XC    DEMOS,DEMOS                                                      
         MVC   DEMOS(18),NPLNDEMS                                               
         MVC   TARGET,DEMOS        FIRST PLAN DEMO IS TARGET                    
*        GOTO1 VSETDB                                                           
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'NAD'                                                   
         MVI   DBSELMED,C'T'                                                    
         GOTO1 DEMOCON,DMCB,(0,TARGET),(10,WORK),(C'S',DBLOCK)                  
         MVC   TARGNAME,WORK                                                    
         MVC   GUARCPM,NPLNGCPM                                                 
         MVC   PLANADJP,NPLNADJP                                                
*        MVC   PLANADJD,NPLNADJD                                                
         MVC   PLANZONE,NPLNZONE                                                
         MVI   LENGTH,0                                                         
         GOTO1 VEXTBUDG                                                         
         SPACE 1                                                                
         XC    PLANDADJ,PLANDADJ   CLEAR IN CASE NO X'05'                       
         XC    GUADEMO,GUADEMO                                                  
         LR    R6,R4                                                            
         MVI   ELCODE,X'05'        NEW GENERAL ELEM                             
         BAS   RE,GETEL                                                         
         BNE   EXTP1                                                            
         USING NPNELEM,R6                                                       
         MVC   PLANDADJ,NPNADJD    NEW 4 DEC DEMO ADJ                           
         MVC   GUADEMO,NPNDEMO     GUARANTEE DEMO CATEGORY                      
         DROP  R6                                                               
         SPACE 1                                                                
EXTP1    LR    R6,R4                                                            
         MVI   ELCODE,X'08'        NEW GENERAL ELEM                             
         BAS   RE,GETEL                                                         
         BNE   EXTP1A                                                           
         USING NPLNEL2,R6                                                       
         MVC   BOOKTVQ,NPL2BOOK    TVQ BOOK                                     
         DROP  R6                                                               
         SPACE 1                                                                
EXTP1A   LR    R6,R4               FORMAT UNIVERSE ELEMENT                      
         MVI   ELCODE,X'02'                                                     
         MVC   PUEL(3),=X'31F344'                                               
         XC    UNIVS,UNIVS                                                      
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING NPUELD,R6                                                        
         ZIC   R1,NPULEN                                                        
         SH    R1,=H'5'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   UNIVS(0),NPUNIVS                                                 
*                                                                               
         MVI   ELCODE,X'DD'        ANY NAD UNIVERSES                            
         BAS   RE,NEXTEL                                                        
         BNE   EXTP2                                                            
         USING NPNADD,R6                                                        
         XC    NADUNIVS,NADUNIVS                                                
         LA    R1,NADUNIVS                                                      
         USING OVERAREA,R1                                                      
EXTP1C   MVC   0(2,R1),=X'DD0C'                                                 
         MVC   OVERDEMO,NPNDDEM                                                 
**       MVC   OVERAMNT+2(2),NPNDVAL                                            
         MVC   OVERAMNT,NPNDVAL                                                 
         MVI   OVERFLG,X'80'                                                    
         MVI   OVERPREC,X'42'                                                   
         LA    R1,12(R1)                                                        
         BAS   RE,NEXTEL                                                        
         BE    EXTP1C                                                           
         DROP  R1                                                               
*                                                                               
EXTP2    CLI   NETMEDIA,C'S'       SYNDICATION                                  
         BE    XIT                                                              
         CLI   NETMAIN,C'Y'        OR MAIN NETWORK                              
         BE    XIT                                                              
         LA    R2,UNIVS                                                         
         LA    R3,60                                                            
         SPACE 1                                                                
EXTPLAN2 L     R1,0(R2)            ADJUST UNIVS FOR NON NETWORK                 
         M     R0,=F'10'                                                        
         ST    R1,0(R2)                                                         
         LA    R2,4(R2)                                                         
         BCT   R3,EXTPLAN2                                                      
         B     XIT                                                              
         SPACE 1                                                                
BADPLAN  MVC   CONHEAD(L'NOPLAN),NOPLAN                                         
         B     MYEND                                                            
         DROP  R4                                                               
         DROP  R6                                                               
         EJECT                                                                  
*              EXTRACT BUDGET FOR PLAN(/LENGTH)                                 
         SPACE 3                                                                
EXTBUDG  L     R6,AIO                                                           
         XC    BUDGETS,BUDGETS                                                  
         USING NPBELD,R6                                                        
         MVI   ELCODE,X'04'        LOOKING FOR BUDGET ELEMENTS                  
         BAS   RE,GETEL                                                         
         B     EXTBUDG4                                                         
         SPACE 1                                                                
EXTBUDG2 BAS   RE,NEXTEL                                                        
         SPACE 1                                                                
EXTBUDG4 BNE   EXTBUDEX                                                         
         CLI   LENGTH,0            IF LENGTH WAS SPECIFIED                      
         BE    EXTBUDG5                                                         
         CLC   NPBSEC,LENGTH       BUDGET MUST MATCH                            
         BNE   EXTBUDG2                                                         
         SPACE 1                                                                
EXTBUDG5 ZIC   R2,NPBPER+1         PICK UP PERIOD NUMBER                        
         CLI   PUPQFLG,C'Y'        IF QUARTER FILTER                            
         BNE   EXTB6               CHECK PUPQFLT AGAINST PERIOD                 
         CH    R2,=H'4'                                                         
         BNE   EXTB5A                                                           
         CLI   PUPQFLT,0                                                        
         BNE   EXTBUDG2                                                         
         B     EXTB6                                                            
EXTB5A   CLC   PUPQFLT,NPBPER+1                                                 
         BNE   EXTBUDG2                                                         
EXTB6    CH    R2,=H'4'                                                         
         BNE   *+6                                                              
         SR    R2,R2                                                            
         MH    R2,=H'20'                                                        
         LA    R2,BUDGETS(R2)      INDEX INTO BUDGETS FOR PERIOD                
         BAS   RE,BUDGADD                                                       
         LA    R2,BUDGETS+80       THEN ADD FOR TOTAL BUDGET                    
         BAS   RE,BUDGADD                                                       
         B     EXTBUDG2                                                         
*                                                                               
EXTBUDEX B     XIT                                                              
         SPACE 1                                                                
BUDGADD  NTR1                                                                   
         L     R1,NPBUDGET                                                      
         A     R1,0(R2)            ADD INTO TOTAL FOR PERIOD                    
         ST    R1,0(R2)                                                         
         CLI   NPBSEC,0            BUDGET IS FOR ALL LENGTHS                    
         BE    XIT                                                              
         LA    R2,4(R2)                                                         
         L     R4,AIO                                                           
         USING NPLRECD,R4                                                       
         LA    R3,NPLNLENS                                                      
         ZIC   R0,NPLNNLEN                                                      
         SPACE 1                                                                
BUDGADD2 CLC   NPBSEC,0(R3)        ANALYZE SPECIFIC BUDGETS                     
         BE    BUDGADD4                                                         
         LA    R2,4(R2)                                                         
         LA    R3,1(R3)                                                         
         BCT   R0,BUDGADD2                                                      
         DC    H'0'                                                             
         SPACE 1                                                                
BUDGADD4 L     R1,NPBUDGET                                                      
         A     R1,0(R2)                                                         
         ST    R1,0(R2)                                                         
         B     XIT                                                              
         DROP  R6                                                               
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO VALIDATE PROGRAM                                      
         SPACE 3                                                                
VALPROG  GOTO1 ANY                                                              
         MVC   PROGCODE,WORK                                                    
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING NPUKEY,R4                                                        
         MVI   NPUKTYPE,X'22'      FILL PROGRAM KEY                             
         MVC   NPUKAM,BINAGYMD                                                  
         MVC   NPUKCLT,CLTCOMP                                                  
         MVC   NPUKNET,NETWORK                                                  
         MVC   NPUKDPT,DPTCODE                                                  
         MVC   NPUKPLAN,PLANCODE                                                
         MVC   NPUKPROG,PROGCODE                                                
         MVC   NPUKPERQ,PROGPERQ                                                
         GOTO1 READ                                                             
         GOTO1 GETREC                                                           
         GOTO1 VEXTPROG                                                         
         B     XIT                                                              
         SPACE 1                                                                
EXTPROG  L     R4,AIO                                                           
         USING NPURECD,R4                                                       
         MVC   PROGCODE,NPUKPROG                                                
         MVC   PROGPERQ,NPUKPERQ                                                
         GOTO1 =A(MORE),DMCB,(RC),6,RR=RELO       EXTPER ROUTINE                
*        BAS   RE,EXTPER           BUILD PERIOD LIST                            
         MVC   PROGNAME,NPGDNAME                                                
         MVC   PROGDAYC,NPGDDAY                                                 
         ZIC   R1,PROGDAYC                                                      
         MH    R1,=H'3'                                                         
         LA    R1,DAYLIST(R1)                                                   
         MVC   PROGDAY,0(R1)                                                    
         MVC   PROGMIL,NPGDTIME                                                 
         MVC   PROGNTI,NPGDNTI                                                  
         XC    PROGTIME,PROGTIME                                                
         GOTO1 UNTIME,DMCB,(PLANZONE,PROGMIL),PROGTIME                          
         MVC   PROGFILT,NPGDFILT                                                
         SPACE 1                                                                
         LR    R6,R4               PUT BOOK ELEMENT INTO EVN                    
         MVI   ELCODE,X'5D'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   PBEL,0(R6)                                                       
         B     XIT                                                              
         DROP  R4                                                               
         SPACE 1                                                                
DAYLIST  DC    C'M-FMONTUEWEDTHUFRISATSUNM-SVAR'                                
         EJECT                                                                  
*              ROUTINE TO LOOK UP PROGRAM DETAILS FROM SPOT FILE                
         SPACE 3                                                                
LUPPROG  CLI   PLANPRCB,C'Y'       CABLE PLAN                                   
         BE    LUPCPRG             GET CABLE PROGRAM                            
         XC    CABDEMS,CABDEMS     NOT CABLE PROGRAM RECORD                     
         GOTO1 VSETSPOT                                                         
         MVC   BLOCK(20),KEY       SAVE PROGRAM KEY                             
         MVC   BLOCK+20(96),DMWORK      AND DMWORK                              
         MVC   AIO,AIO2            WE'LL USE IO2                                
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING NPGRECD,R4                                                       
         MVC   NPGKTYP,=X'0D20'                                                 
         MVC   NPGKAM,BINAGYMD                                                  
         MVC   NPGKNET,NETMKTN                                                  
         MVC   NPGKPROG,PROGCODE                                                
         MVC   NPGKEND,PERIOD                                                   
         CLI   PLANPERT,C'W'                                                    
         BE    LUPPROG3                                                         
         MVC   DUB(2),PERIOD       PRESET TO 1ST OF MONTH                       
         MVI   DUB+2,1                                                          
         CLI   DUB+1,9             EXCEPT SEPTEMBER USE 19TH                    
         BNE   *+8                 (THIS WAS GOOD FOR 1988)                     
         MVI   DUB+2,19            (MAY NEED TO REVISE LATER)                   
         CLI   PLANPERT,C'Q'       IF THIS IS QUARTERLY                         
         BNE   LUPPROG2               USE START OF QUARTER                      
         ZIC   R1,PERIOD+1         PICK UP QUARTER (1-4)                        
         BCTR  R1,0                (0-3)                                        
         MH    R1,=H'3'            (0,3,6,9)                                    
         LA    R1,1(R1)            (1,4,7,10)                                   
         STC   R1,DUB+1                                                         
         CLI   DUB+1,10            IF 4TH QUARTER                               
         BNE   LUPPROG2            LISA                                         
         MVI   DUB+1,11            SAYS USE NOV 1                               
         MVI   DUB+2,1                                                          
         SPACE 1                                                                
LUPPROG2 GOTO1 DATCON,DMCB,(3,DUB),(2,NPGKEND)                                  
         SPACE 1                                                                
LUPPROG3 GOTO1 HIGH                                                             
         CLC   KEY(11),KEYSAVE     SHOULD MATCH ON PROGRAM                      
         BE    LUPPROG4                                                         
         MVC   CONHEAD(L'INVPROG),INVPROG                                       
         B     MYEND                                                            
         SPACE 1                                                                
LUPPROG4 MVC   FILENAME,=C'SPTFILE '                                            
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'92'        MOST DETAILS FROM 92 ELEMENT                 
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NPGELEM,R6                                                       
         MVC   PROGNAME,NPGNAME                                                 
         MVC   PROGDAYC,NPGRDAY                                                 
         MVC   PROGMIL,NPGTIME                                                  
         MVC   PROGFILT,NPGFILT                                                 
         MVC   PROGNTI,NPGPPNO                                                  
         ZIC   R1,PROGDAYC                                                      
         MH    R1,=H'3'                                                         
         LA    R1,DAYLIST(R1)                                                   
         MVC   PROGDAY,0(R1)                                                    
         XC    PROGTIME,PROGTIME                                                
         GOTO1 UNTIME,DMCB,PROGMIL,PROGTIME                                     
         XC    VPHS,VPHS                                                        
         LA    R2,NPGVPHS          MULTIPLY THESE BY 10                         
         LA    R3,VPHS                                                          
         LA    R0,34                                                            
         SPACE 1                                                                
LUPPROG6 ZIC   R1,0(R2)                                                         
         MH    R1,=H'10'                                                        
         STH   R1,0(R3)                                                         
         LA    R2,1(R2)                                                         
         LA    R3,2(R3)                                                         
         BCT   R0,LUPPROG6                                                      
         SPACE 1                                                                
         XC    RATING,RATING                                                    
         MVC   SHARE,NPGSHARE      CAN BE SHARE                                 
         TM    NPGSTAT,X'80'                                                    
         BNO   *+16                                                             
         MVC   RATING,NPGSHARE     OR RATING                                    
         XC    SHARE,SHARE                                                      
         SPACE 1                                                                
         L     R6,AIO              LOOK FOR NEW VPHS                            
         MVI   ELCODE,X'93'                                                     
         BAS   RE,GETEL                                                         
         BNE   LUPPROG8                                                         
         USING NPGEL93,R6                                                       
         XC    VPHS,VPHS                                                        
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'6'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   VPHS(0),NPG2VPHS                                                 
****     STC   R1,VPHSLEN                                                       
         L     R6,AIO                                                           
         MVI   ELCODE,X'DD'        LOOK FOR NAD DEMOS                           
         BAS   RE,GETEL                                                         
         BNE   LUPPROG8                                                         
         USING NUOVEL,R6                                                        
         LA    R2,NOVERS           AND MOVE TO N(AD)OVER AREA                   
LUPPROG7 CLI   NUOVCAT,0           IS DEMO NAD                                  
         BE    LUPPRG7B            NO,GET NEXT OVERRIDE                         
         CLI   NUOVCAT+1,C'V'      IS DEMO NAD                                  
         BNE   LUPPRG7B           NO,GET NEXT OVERRIDE                          
         MVC   0(3,R2),NUOVCAT                                                  
         MVC   3(2,R2),NUOVVAL+2                                                
         LA    R2,5(R2)                                                         
LUPPRG7B BAS   RE,NEXTEL                                                        
         BE    LUPPROG7                                                         
         SPACE 1                                                                
LUPPROG8 L     R6,AIO              PICK UP BOOK ELEMENT                         
         MVI   ELCODE,X'5D'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   PBEL,0(R6)                                                       
         SPACE 1                                                                
         MVC   KEY(20),BLOCK       RESTORE PROGRAM KEY                          
         MVC   AIO,AIO1            RESET TO IO1                                 
         MVC   DMWORK(96),BLOCK+20 RESET DMWORK                                 
         GOTO1 VSETUNT                                                          
         B     XIT                                                              
         DROP  R4                                                               
         DROP  R6                                                               
         EJECT                                                                  
*              ROUTINE TO LOOK UP CABLE PROGRAM DETAILS                         
         SPACE 3                                                                
LUPCPRG  LA    R4,KEY                                                           
         MVC   BLOCK(20),KEY       SAVE PROGRAM KEY                             
         MVC   BLOCK+20(96),DMWORK      AND DMWORK                              
         MVC   AIO,AIO2            WE'LL USE IO2                                
         XC    KEY,KEY                                                          
         USING NPCRECD,R4                                                       
         MVI   NPCKTYPE,X'28'      FILL PLAN KEY                                
         MVC   NPCKAM,BINAGYMD                                                  
         MVC   NPCKCLT,CLTCOMP                                                  
         MVC   NPCKNET,NETWORK                                                  
         MVC   NPCKDPT,DPTCODE                                                  
         MVC   NPCKPLAN,PLANCODE                                                
         GOTO1 HIGH                                                             
         B     LPC40                                                            
         SPACE 1                                                                
LPC20    GOTO1 SEQ                                                              
         SPACE 1                                                                
LPC40    CLC   KEY(15),KEYSAVE     AGY/MED/CLT                                  
         BE    LPC60                                                            
         MVC   CONHEAD(L'INVCPROG),INVCPROG                                     
         B     MYEND                                                            
LPC60    GOTO1 GETREC                                                           
         L     R4,AIO                                                           
*                                                                               
         MVC   DUB(4),PROGCODE                                                  
         OC    DUB(4),SPACES                                                    
         GOTO1 HELLO,DMCB,(C'G',=C'UNTFIL  '),(X'02',AIO),(4,DUB)               
         CLI   12(R1),0            TEST IF OVERRIDE FOUND                       
         BE    *+14                NOT FOUND                                    
         MVC   CONHEAD(L'INVCPROG),INVCPROG                                     
         B     MYEND                                                            
*                                                                               
         L     R6,12(R1)           GET PROGRAM ELEMENT                          
         USING NPPELD,R6                                                        
         MVC   PROGNAME(9),NPPNAME                                              
         MVC   PROGDAYC,NPPDAY                                                  
         MVC   PROGMIL,NPPTIME                                                  
         XC    PROGFILT,PROGFILT                                                
         ZIC   R1,PROGDAYC                                                      
         MH    R1,=H'3'                                                         
         LA    R1,DAYLIST(R1)                                                   
         MVC   PROGDAY,0(R1)                                                    
         XC    PROGTIME,PROGTIME                                                
         GOTO1 UNTIME,DMCB,PROGMIL,PROGTIME                                     
*                                                                               
         XC    RATING,RATING                                                    
         MVC   SHARE,NPPRTSH       CAN BE SHARE                                 
         CLI   NPPRSIN,C'R'                                                     
         BNE   *+16                                                             
         MVC   RATING,NPPRTSH      OR RATING                                    
         XC    SHARE,SHARE                                                      
*                                                                               
         XC    CABDEMS,CABDEMS                                                  
         LA    R2,CABDEMS          CABLE DEMO HOLD AREA                         
         LA    R3,NPPDEM1                                                       
         LA    RE,4                                                             
*                                                                               
LCP200   MVC   0(2,R2),2(R3)       MOVE CABLE DEMOS OUT                         
         LA    R2,2(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   RE,LCP200                                                        
*                                                                               
*  SET BOOK ELEMENT UP                                                          
*                                                                               
         XC    PBEL,PBEL                                                        
         MVC   PBEL(2),=X'5D07'                                                 
         MVC   PBEL+2(3),=C'EVN'                                                
         MVC   PBEL+5(2),=X'580E'                                               
*                                                                               
         MVC   AIO,AIO1            RESET AIO                                    
         MVC   KEY(20),BLOCK       RESTORE PROGRAM KEY                          
         MVC   DMWORK(96),BLOCK+20 RESET DMWORK                                 
         B     XIT                                                              
         DROP  R4                                                               
         DROP  R6                                                               
         EJECT                                                                  
*              ROUTINE TO EXTRACT SHARE HUT RATING                              
*              AND VPHS FOR THIS PROGRAM/PERIOD                                 
EXTDEM   GOTO1 =A(MORE),DMCB,(RC),4,RR=RELO                                     
         EJECT                                                                  
*              ROUTINE TO EXTRACT UNITS FOR PROG/PERIOD/LENGTH                  
         SPACE 3                                                                
EXTUNS   L     R6,AIO                                                           
         MVI   UNITS,0                                                          
         XC    UNTEQV,UNTEQV                                                    
         XC    UNTSEC,UNTSEC                                                    
         SR    R1,R1               SAVE UNITS IN R1                             
         SR    R5,R5               SAVE SECS IN R5                              
         MVI   ELCODE,0                                                         
         BAS   RE,GETEL                                                         
         B     EXTUN4                                                           
         SPACE 1                                                                
EXTUN2   BAS   RE,NEXTEL                                                        
         SPACE 1                                                                
EXTUN4   CLI   0(R6),0                                                          
         BE    XIT                                                              
         CLI   0(R6),X'02'                                                      
         BE    EXTUN6                                                           
         CLI   0(R6),X'03'                                                      
         BNE   EXTUN8                                                           
         B     EXTUN2                                                           
         SPACE 1                                                                
         USING NPUAD,R6                                                         
EXTUN6   CLC   NPUAPER,PERIOD      MATCH ON PERIOD                              
         BNE   EXTUN2                                                           
         LA    R4,NPUAUNS                                                       
         B     EXTUN10                                                          
         SPACE 1                                                                
         USING NPUBD,R6                                                         
EXTUN8   CLC   NPUBPER,PERIOD      MATCH ON PERIOD                              
         BNE   EXTUN2                                                           
         CLI   0(R6),X'12'                                                      
         BNE   EXTUN2                                                           
         LA    R4,NPUBUNS                                                       
         SPACE 1                                                                
EXTUN10  LA    R3,PLANLENS                                                      
         ZIC   R0,PLANNLEN                                                      
         SPACE 1                                                                
EXTUN12  CLI   LENGTH,0            RETURN ALL UNITS IF LENGTH=0                 
         BE    EXTUN14                                                          
         CLC   LENGTH,0(R3)        ELSE CHECK LENGTH MATCH                      
         BNE   EXTUN16                                                          
         SPACE 1                                                                
EXTUN14  ZIC   RF,0(R4)            ADD UP UNITS                                 
         AR    R1,RF                                                            
         LTR   RF,RF                                                            
         BZ    EXTUN16                                                          
         SR    RE,RE                                                            
         ZIC   R2,0(R3)            ADD UP LENGTHS                               
         MR    RE,R2                                                            
         AR    R5,RF                                                            
         SPACE 1                                                                
EXTUN16  LA    R3,1(R3)                                                         
         LA    R4,1(R4)                                                         
         BCT   R0,EXTUN12                                                       
         STC   R1,UNITS            RETURN UNITS                                 
         STH   R5,UNTSEC           RETURN SECONDS LENGTHS                       
         CLI   N0PROF+1,0          IF EQUIVALENCE                               
         BE    XIT                                                              
         SR    R4,R4                                                            
         MH    R5,=H'10'                                                        
         ZIC   RF,N0PROF+1                                                      
         DR    R4,RF                                                            
         STH   R5,UNTEQV           RETURN EQUIVALENCED UNITS                    
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*              ROUTINE TO VALIDATE DEMO STRING                                  
         SPACE 3                                                                
VALDEM   MVI   DEMSREQD,0                                                       
         CLI   5(R2),0             OPTIONAL OVERRIDE                            
         BE    XIT                                                              
         MVI   DEMSREQD,C'Y'                                                    
*        GOTO1 VSETDB                                                           
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'TPT'                                                   
         MVI   DBSELMED,C'T'                                                    
         XC    PARAS(24),PARAS                                                  
         GOTO1 DEMOVAL,PARAS,(R2),(MAX,DEMOS),(C'S',DBLOCK)                     
*        GOTO1 DEMOVAL,PARAS,(R2),(MAX,DEMOS),(0,DBLOCK)                        
         CLI   4(R1),0                                                          
         BE    BADDEM                                                           
         MVC   NDEMOS,4(R1)        PASS USER BACK NUMBER FOUND                  
         B     XIT                                                              
         SPACE 1                                                                
BADDEM   MVC   CONHEAD(L'INVDEM),INVDEM   INVALID DEMO                          
         B     MYEND                                                            
         EJECT                                                                  
*              GET DEMOS VALUES FOR PROG/PERIOD(/LENGTH)                        
         SPACE 3                                                                
*              INPUTS              GDDEMO SET TO REQUIRED DEMO NUMBER           
*              OUTPUTS             FILLS IN GD FIELDS                           
         SPACE 1                                                                
GETDEM   GOTO1 VEXTDEM             FILL EVN FOR PERIOD                          
         GOTO1 VSETDB                                                           
         MVC   WORK(10),DOUTLIST                                                
         MVC   WORK(1),GDDEMO                                                   
         MVC   WORK+3(1),GDDEMO                                                 
         MVC   WORK+6(1),GDDEMO                                                 
         MVC   WORK+2(1),GDDEMO+2                                               
         MVC   WORK+5(1),GDDEMO+2                                               
         MVC   WORK+8(1),GDDEMO+2                                               
******   CLI   NETMAIN,C'Y'        FOR MAIN NETWORKS                            
******   BNE   *+8                                                              
         MVI   WORK+8,1            WE NEED TO WORK OUT IMPS                     
         MVI   WORK+6,0            (GET REGULAR HOMES-NOT NAD)                  
         XC    GDAREA,GDAREA       PRECLEAR ACCUMULATOR AREA                    
**       GOTO1 =V(PRNTBL),DMCB,=C'IVPH',VPHS,C'DUMP',240,=C'1D'                 
**       GOTO1 =V(PRNTBL),DMCB,=C'IUNIV',UNIVS,C'DUMP',240,=C'1D'               
         GOTO1 DEMOUT,DMCB,(C'L',WORK),DBLOCK,GDVPH                             
         OC    DBEXTEND,DBEXTEND                                                
*        BZ    *+6                                                              
*        DC    H'0'                                                             
**       GOTO1 =V(PRNTBL),DMCB,=C'OGDVPH',GDVPH,C'DUMP',32,=C'1D'               
         SPACE 1                                                                
         BAS   RE,COMPDEM          CONVERT GDIMP TO DEMO IMPRESSIONS            
         CLI   GDNOADJ,C'Y'                                                     
         BE    *+8                                                              
         BAS   RE,ANYADJ           CHECK FOR PACKAGE OR DEMO ADJUST             
         SPACE 1                                                                
         CLI   LENGTH,0                                                         
         BE    GETDEM2             IF LENGTH IS SPECIFIED                       
         BAS   RE,GETDEMLN         GET DEMOS FOR THAT LENGTH                    
         B     GETDEM6                                                          
         SPACE 1                                                                
GETDEM2  LA    R2,PLANLENS         ELSE GO FOR EACH LENGTH                      
         ZIC   R3,PLANNLEN                                                      
         SPACE 1                                                                
GETDEM4  MVC   LENGTH,0(R2)                                                     
         BAS   RE,GETDEMLN                                                      
         LA    R2,1(R2)                                                         
         BCT   R3,GETDEM4                                                       
         MVI   LENGTH,0                                                         
         SPACE 1                                                                
GETDEM6  CLI   GDRAWOPT,C'Y'       UNLESS RAW OPTION IS SET                     
         BE    XIT                                                              
         MVC   GDTGRP(8),GDEGRP    PASS BACK EQUIVALENCED                       
         B     XIT                                                              
         EJECT                                                                  
*              ADD TOTAL AND EQUIV DEMOS FOR LENGTH                             
         SPACE 3                                                                
GETDEMLN NTR1                                                                   
         MVC   EQUIMP,GDIMP        CREATE EQUIV IMPS                            
         LA    R2,EQUIMP                                                        
         BAS   RE,EQUIVI                                                        
         MVC   EQUGRP,GDGRP               AND GRPS                              
         LA    R2,EQUGRP                                                        
         BAS   RE,EQUIVR                                                        
         LA    R2,EQUIMP           ROUND EQUIV IMPRESSIONS                      
         CLI   GDRNDOPT,C'Y'                                                    
         BE    *+8                                                              
         BAS   RE,ROUND                                                         
         MVC   RAWIMP,GDIMP        CREATE RAW IMPS                              
         MVC   RAWGRP,GDGRP               AND GRPS                              
         LA    R2,RAWIMP           ROUND IMPRESSIONS                            
         CLI   GDRNDOPT,C'Y'                                                    
         BE    *+8                                                              
         BAS   RE,ROUND                                                         
         GOTO1 VEXTUNS             GET UNITS FOR PERIOD(/LENGTH)                
         ZIC   R2,UNITS            ADD UNITS INTO TOTAL                         
         LR    R1,R2                                                            
         A     R1,GDUNITS                                                       
         ST    R1,GDUNITS                                                       
         L     R1,RAWGRP           COMPUTE TOTAL GRPS                           
         MR    R0,R2                                                            
         A     R1,GDTGRP                                                        
         ST    R1,GDTGRP                                                        
         L     R1,RAWIMP               AND TOTAL IMPS                           
         MR    R0,R2                                                            
         A     R1,GDTIMP                                                        
         ST    R1,GDTIMP                                                        
         L     R1,EQUGRP           COMPUTE EQUIV GRPS                           
         MR    R0,R2                                                            
         A     R1,GDEGRP                                                        
         ST    R1,GDEGRP                                                        
         L     R1,EQUIMP               AND EQUIV IMPS                           
         MR    R0,R2                                                            
         A     R1,GDEIMP                                                        
         ST    R1,GDEIMP                                                        
         B     XIT                                                              
         SPACE 1                                                                
DOUTLIST DC    X'00',C'V',X'00'    DEMOUT LIST FOR VPH                          
         DC    X'00',C'R',X'00'                    RATINGS                      
         DC    X'00',C'H',X'00'                AND IMPS                         
         DC    X'FF'                                                            
         EJECT                                                                  
*              SUBSIDIARY ROUTINES FOR GETDEM                                   
         SPACE 3                                                                
COMPDEM  NTR1                                                                   
*                                  COMPUTE DEMOGRAPHIC IMPS                     
******   CLI   NETMAIN,C'Y'        ONLY NEEDED FOR MAIN NETWORK                 
******   BNE   XIT                                                              
         CLI   GDDEMO+2,1            AND IF WE ARE NOT DOING HOMES              
*        CLI   GDDEMO,1            AND IF WE ARE NOT DOING HOMES                
         BE    XIT                                                              
         OC    GDVPH,GDVPH                                                      
         BNZ   *+14                                                             
         XC    GDIMP,GDIMP                                                      
         B     XIT                                                              
         L     R0,GDVPH            GDIMP STARTED OFF WITH HOMES IMPS            
         L     R1,GDIMP                                                         
         LA    R2,GDIMP                                                         
         BAS   RE,ROUND            ROUND THE HOMES (CONVERTS TO 000)            
         MR    R0,R0                                                            
         AH    R1,=H'500'                                                       
         D     R0,=F'1000'                                                      
         ST    R1,GDIMP            RETURN DEMO IMPRESSIONS (IN 00)              
         B     XIT                                                              
         SPACE 1                                                                
EQUIVR   NTR1                                                                   
         ZIC   RF,N2PROF           EQUIVALENCE RATINGS                          
         B     EQUIVALL                                                         
         SPACE 1                                                                
EQUIVI   NTR1                                                                   
         ZIC   RF,N0PROF+1         EQUIVALENCE IMPS                             
         SPACE 1                                                                
EQUIVALL LTR   RF,RF               EQUIVALENCE RATINGS                          
         BZ    XIT                 (IF NECESSARY)                               
         L     R1,0(R2)            R2=A(IMPS OR RATINGS)                        
         ZIC   R0,LENGTH           LENGTH=PRESENT LENGTH                        
         MR    R0,R0                                                            
         SLDA  R0,1                                                             
         DR    R0,RF                                                            
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         ST    R1,0(R2)                                                         
         B     XIT                                                              
         SPACE 1                                                                
ROUND    NTR1                                                                   
         L     R1,0(R2)            R2=A(IMPRESSIONS TO BE ROUNDED)              
         CLI   NETMAIN,0                                                        
         BE    *+8                                                              
         CLI   NETMEDIA,C'S'       SYNDICATION                                  
         BE    *+8                                                              
         CLI   NETMAIN,C'Y'        OR MAIN NETWORK                              
         BNE   ROUND2                                                           
         AH    R1,=H'50'                                                        
         SR    R0,R0                                                            
         D     R0,=F'100'                                                       
         M     R0,=F'10'           (RETURN MAIN IN 000 TO NEAREST               
         ST    R1,0(R2)                                   10000)                
         B     XIT                                                              
         SPACE 1                                                                
ROUND2   AH    R1,=H'5'                                                         
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
         ST    R1,0(R2)                                                         
         B     XIT                                                              
         EJECT                                                                  
* READ STATION RECORD                                                           
*                                                                               
GETSTAT  NTR1                                                                   
         GOTO1 =A(MORE),DMCB,(RC),0,RR=RELO                                     
         B     XIT                                                              
         SPACE 2                                                                
*              ADJUST DEMOS BY GUARANTEED FACTORS                               
         SPACE 3                                                                
ANYADJ   NTR1                                                                   
         GOTO1 =A(MORE),DMCB,(RC),1,RR=RELO                                     
         B     XIT                                                              
*              ROUTINE TO LOOK UP HUTS FOR PROGRAM/PERIOD                       
         SPACE 3                                                                
LUPHUT   DS    0H                                                               
         GOTO1 =A(MORE),DMCB,(RC),2,RR=RELO                                     
         B     XIT                                                              
*              ROUTINE TO LOOK UP UNIV FOR PROGRAM/PERIOD                       
         SPACE 3                                                                
LUPUNIV  DS    0H                                                               
         GOTO1 =A(MORE),DMCB,(RC),3,RR=RELO                                     
         B     XIT                                                              
         EJECT                                                                  
*              POSITION CURSOR TO CORRECT FIELD IN ERRORS                       
         SPACE 3                                                                
*              INPUTS              R2=A(SCREEN HEADER)                          
*                                  FIELDERR=NUMBER OF FIELD IN ERROR            
         SPACE 1                                                                
CURSERR  CLI   FIELDERR,0          APPLICATION MUST SET FIELD NUMBER            
         BE    ERRXIT                                                           
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
         B     ERRXIT                                                           
         EJECT                                                                  
*              SET UP AND ERRORS                                                
         SPACE 3                                                                
VVSETUNT MVC   LKEY,=H'20'                                                      
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'27'                                                  
         XC    FILENAME,FILENAME                                                
         B     XIT                                                              
         SPACE 1                                                                
VVSETSPT MVC   LKEY,=H'13'                                                      
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'24'                                                  
         MVC   FILENAME,=CL8'SPTDIR'                                            
         B     XIT                                                              
         SPACE 1                                                                
SETDB    GOTO1 =A(MORE),DMCB,(RC),5,RR=RELO       GETTVQ                        
*                                                                               
         XC    DBLOCK,DBLOCK       INITIALIZE DBLOCK FOR EVN                    
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'EVN'                                                   
*        MVC   DBFILE,=C'NAD'                                                   
         MVI   DBSELSRC,C'N'                                                    
         MVI   DBSELMED,C'V'                                                    
*        MVI   DBSELMED,C'T'                                                    
         MVC   DBSELAGY,AGENCY                                                  
         MVI   DBFUNCT,DBGETDEM                                                 
         MVC   DBSELSTA+4(1),NETMEDIA                                           
*  SET FOR TVQ LOOKUP                                                           
         XC    DBEXTEND,DBEXTEND                                                
         LA    RF,BUFF                                                          
***      LA    RF,4000(RF)                                                      
         A     RF,=F'4500'                                                      
         USING TVQBLOCK,RF                                                      
         CLI   GOTUTYPE,1                                                       
         BNE   SETDB10                                                          
         MVC   BTUEXT,=C'UFIL'                                                  
         MVC   BTUEX2,BTUDB+4                                                   
         LA    RE,BTUEXT                                                        
         ST    RE,DBEXTEND                                                      
         DROP  RF                                                               
*                                                                               
SETDB10  LA    R1,PIO                                                           
         ST    R1,DBAREC                                                        
         LA    R1,PIO+22                                                        
         ST    R1,DBAQUART                                                      
         B     XIT                                                              
         SPACE 1                                                                
ERRXIT   CLI   ERROR,X'FE'                                                      
         BE    ERRX2                                                            
         SPACE 1                                                                
         GOTO1 ERREX               SYSTEM MESSAGE                               
         SPACE 1                                                                
ERRX2    GOTO1 ERREX2              MY OWN ERROR MSG                             
         EJECT                                                                  
*              SUPPORTING SUBROUTINES                                           
         SPACE 3                                                                
BUMP     ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BR    RE                                                               
         SPACE 3                                                                
         GETEL (R6),DATADISP,ELCODE                                             
         SPACE 1                                                                
MYCURSOR MVI   ERROR,X'FE'         USING MY OWN ERROR MSG                       
         GOTO1 VCURSERR            AND POSITIONING CURSOR                       
         SPACE 1                                                                
MYEND    MVI   ERROR,X'FE'         USING MY OWN ERROR MSG                       
         SPACE 1                                                                
ERREND   GOTO1 VERRXIT                                                          
         SPACE 1                                                                
XIT      XIT1                                                                   
         SPACE 1                                                                
SPARE    DC    H'0'                                                             
         EJECT                                                                  
*              ROUTINES FOR OWN TITLES                                          
         SPACE 3                                                                
VALTITL  XC    OWNTITLE,OWNTITLE   CHECK OWN TITLE INPUT                        
         CLI   5(R2),0             ANY INPUT IN THIS FIELD?                     
         BE    XIT                                                              
         GOTO1 ANY                 MOVE IT TO WORK                              
         MVC   OWNTITLE,WORK       AND SAVE                                     
         B     XIT                                                              
         SPACE 1                                                                
TITOUT   MVC   WORK,UPTITLE        USE STANDARD                                 
         CLI   OWNTITLE,0                                                       
         BE    *+10                                                             
         MVC   WORK,OWNTITLE       OR OWN TITLE                                 
         MVC   H1(40),WORK         MOVE IN TITLE                                
*                                  AND UNDERLINE IT                             
         GOTO1 UNDERLIN,DMCB,H1,(X'BF',H2)                                      
         B     XIT                                                              
         EJECT                                                                  
*              STANDARD HEADLINE ROUTINES                                       
         SPACE 3                                                                
UPHEAD   GOTO1 VTITOUT             DEAL WITH TITLES (ABOVE)                     
         MVC   H4(6),=C'CLIENT'                                                 
         MVC   H4+8(3),CLTCODE                                                  
         MVC   H4+13(20),CLTNAME                                                
         MVC   H5(6),=C'NETWORK'                                                
         MVC   H5+8(4),NETWORK                                                  
         MVC   H5+16(7),=C'DAYPART'                                             
         MVC   H5+24(7),DPTNAME                                                 
         MVC   H6(4),=C'PLAN'                                                   
         MVC   H6+8(4),PLANCODE                                                 
         MVC   H6+13(7),=C'YEAR 19'                                             
         EDIT  (1,PLANYEAR),(2,H6+20)                                           
         EJECT                                                                  
*              DEAL WITH HEADINGS AND BOXES                                     
         SPACE 3                                                                
RH6      OC    UPTITA,UPTITA                                                    
         BZ    *+10                                                             
         MVC   H9,UPTITA                                                        
         OC    UPTITB,UPTITB                                                    
         BZ    *+10                                                             
         MVC   H10,UPTITB                                                       
         OC    UPTITC,UPTITC                                                    
         BZ    *+10                                                             
         MVC   H11,UPTITC                                                       
         L     R4,ABOX             HANDLE BOXES IF WE'RE OFF LINE               
         USING BOXD,R4                                                          
         LTR   R4,R4                                                            
         BZ    XIT                                                              
         CLI   BOXOPT,C'N'         GLOBAL OPTION TO SUPPRESS                    
         BE    XIT                                                              
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
         MVC   BOXROWS,SPACES                                                   
         MVC   BOXCOLS,UPCOLS                                                   
         MVI   BOXROWS+7,C'T'                                                   
         MVI   BOXROWS+58,C'B'                                                  
         LA    R3,BOXROWS+11                                                    
         CLC   H11,SPACES                                                       
         BNE   RH8                                                              
         BCTR  R3,0                                                             
         CLC   H10,SPACES                                                       
         BNE   RH8                                                              
         BCTR  R3,0                                                             
         SPACE 1                                                                
RH8      MVI   0(R3),C'M'                                                       
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE FILTERS                                                 
         SPACE 3                                                                
VALFILT  XC    FILTERS,FILTERS                                                  
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         GOTO1 SCANNER,DMCB,(R2),(8,BLOCK),0                                    
         ZIC   R0,4(R1)                                                         
         LA    R3,BLOCK                                                         
         MVI   FIELDERR,1                                                       
         LA    R4,FILTERS                                                       
         LTR   R0,R0                                                            
         BZ    BADFILT                                                          
         SPACE 1                                                                
VALFILT2 ZIC   R1,0(R3)                                                         
         CH    R1,=H'3'                                                         
         BL    *+8                                                              
         LA    R1,3                                                             
         LTR   R1,R1                                                            
         BZ    FILTEND                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),12(R3)      SAVE THIS FILTER                             
         LA    R4,3(R4)            AND ADDRESS THE NEXT AREA                    
         SPACE 1                                                                
FILTEND  LA    R3,32(R3)                                                        
         AI    FIELDERR,1                                                       
         BCT   R0,VALFILT2                                                      
         B     XIT                                                              
         SPACE 1                                                                
BADFILT  MVC   CONHEAD(L'INVFLT),INVFLT                                         
         B     MYCURSOR                                                         
         EJECT                                                                  
*              ROUTINE TO CHECK FILTERS                                         
         SPACE 3                                                                
*              INPUT               P1=A(FIELD TO BE FILTERED)                   
*                                  FILTERS HAS UP TO 10                         
*                                  3 BYTE FILTER EXPRESSIONS                    
*              OUTPUT              RETURNS CONDITION CODE                       
         SPACE 1                                                                
CHEFILT  L     R4,0(R1)            CHECK FILTERS                                
         LA    R2,FILTERS          CHECK FILTERS                                
         OC    0(3,R2),0(R2)       ARE ANY SPECIFIED?                           
         BZ    CHEFYES             NO - SO ITS OK                               
         LA    R0,10               UP TO TEN MAY BE SPECIFIED                   
         SPACE 1                                                                
CHEF2    OC    0(3,R2),0(R2)       IF ANY IS SPECIFIED                          
         BZ    CHEFNO                                                           
         BAS   RE,CHEF4            GO CHECK                                     
         BE    CHEFYES             FOR ANY ONE TO BE SATISFIED                  
         LA    R2,3(R2)                                                         
         BCT   R0,CHEF2                                                         
         B     CHEFNO              NONE PASSED SO NO GOOD                       
         SPACE 1                                                                
CHEF4    NTR1                                                                   
*                                  R2=A(THIS FILTER EXPRESSION)                 
         LR    R3,R4                                                            
         LA    R0,3                                                             
         SPACE 1                                                                
CHEF6    CLI   0(R2),C'*'          * IS WILD                                    
         BE    CHEF10                                                           
         CLI   0(R2),0             SO IS ZERO                                   
         BE    CHEF10                                                           
         CLI   0(R2),C'?'          QUESTION SIGN IS SPECIAL CHAR MATCH          
         BE    CHEF8                                                            
         CLC   0(1,R2),0(R3)       MUST MATCH                                   
         BNE   CHEFNO                                                           
         B     CHEF10                                                           
         SPACE 1                                                                
CHEF8    CLI   0(R3),C' '          MATCH ON ANY SPECIAL CHARACTER               
         BH    CHEFNO              INCLUDING SPACE AND BINARY ZERO              
         SPACE 1                                                                
CHEF10   LA    R2,1(R2)                                                         
         LA    R3,1(R3)                                                         
         BCT   R0,CHEF6                                                         
         SPACE 1                                                                
CHEFYES  SR    R1,R1                                                            
         B     *+8                                                              
         SPACE 1                                                                
CHEFNO   LA    R1,1                                                             
         LTR   R1,R1                                                            
         B     XIT                                                              
         EJECT                                                                  
*              CONSTANTS, TABLES ETC                                            
         SPACE 3                                                                
RELO     DS    A                                                                
PATCH    DS    0H                  PATCH AREA                                   
         DC    XL32'00'                                                         
SYSVCON  DS    0F                                                               
         DC    V(DUMMY)                                                         
NVTYPES  EQU   (*-SYSVCON)/4                                                    
         SPACE 1                                                                
ADTAB    DS    0AL3                                                             
NADCONS  EQU   (*-ADTAB)/L'ADTAB                                                
         SPACE 3                                                                
CORETAB  DS    0X                  TABLE OF CORE-RESIDENT MODULES               
         DC    X'1415E02630333217'                                              
CORES    EQU   (*-CORETAB)                                                      
         EJECT                                                                  
*              LTORG FOR CONTROLLER                                             
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ERROR MESSAGES FOR CONTROLLER                                    
         SPACE 3                                                                
INVDEM   DC    C'** ERROR ** INVALID DEMO'                                      
INVNET   DC    C'** ERROR ** INVALID NETWORK'                                   
INVDPT   DC    C'** ERROR ** INVALID DAYPART'                                   
INVPROG  DC    C'** ERROR ** NO NETPAK PROGRAM'                                 
INVCPROG DC    C'** ERROR ** NO CABLE PROGRAM'                                  
INVFLT   DC    C'** ERROR ** INVALID FILTER EXPRESSION'                         
INVALL   DC    C'** ERROR ** ALL INVALID'                                       
NOPLAN   DC    C'** ERROR ** PLAN NOT FOUND'                                    
SECERR   DC    C'** ERROR ** ACCESS TO THIS CLIENT NOT AUTHORIZED'              
         EJECT                                                                  
*                                                                               
* EXTRA ROUTINES KEPT IN THIS SECTION                                           
*                                                                               
MORE     DS    0H                                                               
         NMOD1 0,**MORE**,RR=R2                                                 
         L     RC,0(R1)                                                         
         L     R7,ATWA                                                          
         L     R8,ASPOOLD                                                       
         L     R9,ASYSD                                                         
         L     R5,4(R1)                                                         
         MH    R5,=H'4'                                                         
         B     ROUTINES(R5)                                                     
MOX      XMOD1                                                                  
*                                                                               
ROUTINES B     RGETSTAT                 ROUTINE 0                               
         B     RANYADJ                          1                               
         B     RLUPHUT                          2                               
         B     RLUPUNIV                         3                               
         B     EXTDEMS                          4                               
         B     GETTVQS                          5                               
         B     EXTPER                           6                               
         EJECT                                                                  
* READ STATION RECORD                                                           
*                                                                               
RGETSTAT DS    0H                                                               
         MVC   WORK(30),KEY        SAVE CURRENT KEY                             
         XC    KEY,KEY                                                          
*                                                                               
         MVC   KEY(17),=C'SNABC NAA00000000'                                    
         USING STAREC,R4                                                        
         LA    R4,KEY                                                           
         MVC   STAKCALL(4),WORK+5                                               
         MVC   STAKAGY,AGENCY                                                   
         MVI   USEIO,C'Y'                                                       
         MVC   FILENAME,=C'STATION'                                             
         GOTO1 READ                                                             
         PACK  DUB,SMKT            CONVERT 'MARKET' NUMBER                      
         CVB   R1,DUB                                                           
         STH   R1,DUB                                                           
         MVC   NETMKTN,DUB                                                      
         MVC   NETMEDIA,STYPE                                                   
         MVI   USEIO,C'N'                                                       
         XC    FILENAME,FILENAME                                                
         MVC   KEY(30),WORK        RESET KEY                                    
         GOTO1 HIGH                                                             
         GOTO1 GETREC              REREAD PLAN RECORD                           
         B     MOX                                                              
         EJECT                                                                  
*              ADJUST DEMOS BY GUARANTEED FACTORS                               
*                                                                               
RANYADJ  DS    0H                                                               
         MVC   FULL,PLANADJP                                                    
         L     R2,FULL            PICK UP ALL ADJUSTMENT FACTOR                 
         MVC   FULL,=F'500000'                                                  
****     CLC   GDDEMO,TARGET+2     OR IF SELECTED DEMO IS TARGET                
         OC    GUADEMO,GUADEMO     IS DEMO GUARANTEE SET                        
         BZ    ANYADJ20                                                         
         CLC   GDDEMO,GUADEMO      IS THIS THE RIGHT CATEGORY                   
         BE    ANYADJ30                                                         
         B     ANYADJ40                                                         
ANYADJ20 CLC   GDDEMO,TARGET       OR IF SELECTED DEMO IS TARGET                
         BNE   ANYADJ40                                                         
ANYADJ30 OC    PLANDADJ,PLANDADJ   AND THERE IS A DEMO FACTOR                   
         BZ    ANYADJ40                                                         
         MVC   FULL,PLANDADJ                                                    
         L     R2,FULL             DEMO ADJUSTMENT FACTOR                       
         MVC   FULL,=F'500000'                                                  
         SPACE 1                                                                
ANYADJ40 LTR   R2,R2                                                            
         BZ    MOX                                                              
         L     R1,GDGRP            ADJUST GRPS                                  
         MR    R0,R2                                                            
*        D     R0,=F'5000'                                                      
         D     R0,FULL                                                          
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         ST    R1,GDGRP                                                         
         SPACE 1                                                                
ANYADJ50 L     R1,GDIMP            ADJUST IMPRESSIONS                           
         MR    R0,R2                                                            
*        D     R0,=F'5000'                                                      
         D     R0,FULL   '                                                      
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         ST    R1,GDIMP                                                         
         B     MOX                                                              
         EJECT                                                                  
*              ROUTINE TO LOOK UP HUTS FOR PROGRAM/PERIOD                       
         SPACE 3                                                                
RLUPHUT  DS    0H                                                               
         CLI   LOOKUPSW,C'N'       OPTION NOT TO LOOK UP                        
         BE    LUPHUT6                                                          
         LA    R4,HUTBLOCK                                                      
         USING GETHUTD,R4                                                       
         XC    GHBLOCK,GHBLOCK                                                  
         MVC   GHREPDAY,PROGDAYC                                                
         MVC   GHMILTIM,PROGMIL                                                 
         MVI   GHSCHEME,X'FE'      PRESET FOR YEAR RECORDS                      
         MVI   GH52,C'Y'           AND FOR 52 WEEK HUTS                         
***      CLI   N1PROF+3,0          PROFILE OPTION ON 52 WEEKS                   
***      BE    *+10                                                             
***      MVC   GH52,N1PROF+3                                                    
         MVC   GHDATE,PERIOD       USE ACTUAL DATE FOR WEEKLIES                 
         CLI   PLANPERT,C'W'                                                    
         BE    LUPHUT3                                                          
         MVC   DUB(2),PERIOD       PRESET TO 15TH OF MONTH                      
         MVI   DUB+2,15                                                         
         CLI   PLANPERT,C'Q'       UNLESS THIS IS QUARTERLY                     
         BNE   LUPHUT2                                                          
         ZIC   R1,PERIOD+1         PICK UP QUARTER (1-4)                        
         BCTR  R1,0                (0-3)                                        
         MH    R1,=H'3'            (0,3,6,9)                                    
         LA    R1,2(R1)            (2,5,8,11)                                   
         STC   R1,DUB+1                                                         
         SPACE 1                                                                
LUPHUT2  GOTO1 DATCON,DMCB,(3,DUB),(2,GHDATE)                                   
         SPACE 1                                                                
LUPHUT3  MVC   GHAVE,PLANHTAV                                                   
         MVC   GHYEAR,PLANHTYR                                                  
         ZIC   R1,GHYEAR            CHECK FOR Y2K                               
         CH    R1,=H'50'                                                        
         BH    *+12                                                             
         LA    R1,100(R1)                                                       
         STCM  R1,1,GHYEAR                                                      
         MVC   GHNYEARS,PLANHTNO                                                
         MVC   GHBKTYPE,PLANHTYP                                                
         MVC   GHFLAVOR,N2PROF+2   HUT FLAVOR COMES FROM N2 PROFILE             
         CLI   PLANHTFL,0          BUT CAN BE OVERRIDDEN                        
         BE    *+10                                                             
         MVC   GHFLAVOR,PLANHTFL                                                
         CLI   PLANHTSC,0          IS THERE AN AGENCY SCHEME                    
         BE    LUPHUT4                                                          
         MVC   GHSCHEME,PLANHTSC   THEN USE THIS                                
         MVC   GHAGYMED,BINAGYMD   PASS AGENCY/MEDIA                            
         MVC   GHDEFDEM,N1PROF+4   OPTION TO DEFAULT TO DDS                     
         SPACE 1                                                                
LUPHUT4  MVC   GHCOMFCS,ACOMFACS                                                
         MVC   GHNETWK,NETWEEK                                                  
         GOTO1 GETHUT,DMCB,(R4)                                                 
         MVC   HUT,GHHUT                                                        
         OC    PLANHTPO,PLANHTPO   POSSIBLE PERCENT OVERRIDE                    
         BZ    LUPHUT6                                                          
         SR    R1,R1                                                            
         ICM   R1,3,HUT            YES - SO APPLY                               
         LH    R0,PLANHTPO                                                      
         MR    R0,R0                                                            
         D     R0,=F'5000'                                                      
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         STCM  R1,3,HUT                                                         
         SPACE 1                                                                
LUPHUT6  OC    SHARE,SHARE         COMPUTE RATING                               
         BZ    XIT                                                              
         SR    R0,R0                                                            
         ICM   R0,3,HUT                                                         
         SR    R1,R1                                                            
         ICM   R1,3,SHARE                                                       
         MR    R0,R0                                                            
         D     R0,=F'500'                                                       
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         STCM  R1,3,RATING                                                      
         B     MOX                                                              
         EJECT                                                                  
*              LOOK UP UNIVERSES FOR PLAN                                       
         SPACE 3                                                                
*                                  ROUTINE ASSUMES PLAN IS IN IO1               
*                                  IT USES IO2 FOR RECORD AREA                  
*                                  NPLNUNIV MUST BE SET BY NOW                  
*                                  ELEMENT WILL BE ADDED TO PLAN                
         SPACE 1                                                                
RLUPUNIV LA    R5,BLOCK                                                         
         USING GUVD,R5                                                          
         L     R4,AIO                                                           
         MVI   ELCODE,X'02'        GET RID OF PREVIOUS                          
         GOTO1 REMELEM                                                          
         MVI   ELCODE,X'DD'        GET RID OF PREVIOUS NADS                     
         GOTO1 REMELEM                                                          
         USING NPLRECD,R4                                                       
         XC    GUVBLOCK,GUVBLOCK                                                
         MVC   GUVAGY,AGENCY                                                    
         MVC   DUB(1),NPLNYEAR                                                  
         MVC   DUB+1(2),=X'0101'                                                
         GOTO1 DATCON,DMCB,(3,DUB),(2,GUVDATE)                                  
         SPACE 1                                                                
         LA    R6,ELEMENT          FILL UNIVERSE ELEMENT                        
         USING NPUELD,R6                                                        
         MVI   NPUELEM,X'02'                                                    
         MVI   NPULEN,100                                                       
         MVI   NPUTYPE,C'N'                                                     
         LA    R1,NPUNIVS                                                       
         ST    R1,GUVAOUT                                                       
         MVI   GUVTYPE,2           (HUNDREDS)                                   
         MVC   GUVAREC,AIO2                                                     
         MVC   GUVCMFCS,ACOMFACS                                                
         GOTO1 GETNUN,DMCB,GUVBLOCK                                             
         SPACE 1                                                                
         OC    NPLNUNIV,NPLNUNIV   SPECIAL UNIV CODE?                           
         BZ    LUPUN2                                                           
         MVC   GUVCODE,NPLNUNIV                                                 
         XC    GUVDATE,GUVDATE                                                  
         GOTO1 GETNUN,DMCB,GUVBLOCK                                             
         SPACE 1                                                                
LUPUN2   MVI   ERROR,X'FF'                                                      
         CLI   GUVERROR,0          IF GETNUN WAS NOT HAPPY                      
         BNE   MOX                 PASS BACK ERROR OF X'FF'                     
         MVI   ERROR,0                                                          
         CLI   DMCB,25             GETNUN PASSES BACK N'ENTRIES                 
         BL    LUPUN4                                                           
         ZIC   R1,DMCB                                                          
         SLL   R1,2                                                             
         LA    R1,4(R1)                                                         
         STC   R1,NPULEN           SO ADJUST ELEMENT LENGTH                     
         SPACE 1                                                                
LUPUN4   GOTO1 ADDELEM             AND ADD ELEMENT                              
         SPACE 1                                                                
         DS    0H                  NOW DEAL WITH ANY NAD UNIV ELEMS             
         OC    GUVAREC,GUVAREC                                                  
         BZ    LUPUN50                                                          
         GOTO1 VSETSPOT                                                         
         L     R6,GUVAREC                                                       
         USING NUOVD,R6                                                         
         MVI   ELCODE,X'DD'                                                     
         BAS   RE,GETEL                                                         
         BNE   LUPUN50                                                          
         XC    ELEMENT,ELEMENT                                                  
         LA    R3,ELEMENT                                                       
LUPUN10  MVI   0(R3),X'DD'                                                      
         MVI   1(R3),12                                                         
         MVC   2(3,R3),NUOVCAT     DAT/MODIF/DEMO                               
         MVC   5(4,R3),NUOVVAL     VALUE                                        
         LA    R3,12(R3)                                                        
         BAS   RE,NEXTEL                                                        
         BE    LUPUN10                                                          
         LA    R3,ELEMENT                                                       
LUPUN12  GOTO1 HELLO,DMCB,(C'P',SYSFIL),AIO,0(R3),0                             
         LA    R3,12(R3)                                                        
         CLI   0(R3),X'DD'                                                      
         BE    LUPUN12                                                          
LUPUN50  DS    0H                                                               
         GOTO1 VSETUNT                                                          
         B     MOX                                                              
         DROP  R5                                                               
         DROP  R6                                                               
         EJECT                                                                  
*              ROUTINE TO EXTRACT SHARE HUT RATING                              
*              AND VPHS FOR THIS PROGRAM/PERIOD                                 
         SPACE 3                                                                
EXTDEMS  L     R6,AIO                                                           
         XC    PVEL,PVEL                                                        
         MVC   PVEL(3),=X'33F342'                                               
         XC    PREL,PREL                                                        
*        XC    OVERAREA,OVERAREA                                                
         LA    RE,OVERAREA                                                      
         LA    RF,OVARLENE                                                      
         XCEF                                                                   
         MVC   OVERAREA(L'NADUNIVS),NADUNIVS       SET IN NAD UNIVERSES         
         MVC   PREL(3),=X'350902'                                               
         MVI   ELCODE,0                                                         
         USING NPUAD,R6                                                         
         BAS   RE,GETEL                                                         
         B     EXTDEM4                                                          
         SPACE 1                                                                
EXTDEM2  BAS   RE,NEXTEL                                                        
         SPACE 1                                                                
EXTDEM4  BNE   MOX                                                              
         CLI   0(R6),X'12'         LOOK FOR X'12'                               
         BE    EXTDNEW                  DEAL WITH THEM BELOW                    
         CLI   0(R6),X'02'         LOOK FOR X'02'                               
         BNE   EXTDEM2                                                          
         CLC   NPUAPER,PERIOD                                                   
         BNE   EXTDEM2                                                          
         MVC   RATING,NPUARTG                                                   
         MVC   HUT,NPUAHUT                                                      
         MVC   SHARE,NPUASHR                                                    
         XC    VPHS,VPHS                                                        
         MVC   VPHS(48),NPUAVPHS                                                
         CLI   NPUALEN,72                                                       
         BE    EXTDEM6                                                          
         MVC   VPHS(64),NPUAVPHS                                                
         CLI   NPUALEN,88                                                       
         BE    EXTDEM6                                                          
         MVC   VPHS,NPUAVPHS                                                    
         SPACE 1                                                                
EXTDEM6  MVC   WORK,VPHS           CHANGE VPH SCALE                             
         LA    R2,WORK                                                          
         LA    R3,VPHS                                                          
         LA    R0,64                                                            
         SPACE 1                                                                
EXTDEM8  ZIC   R1,0(R2)                                                         
         MH    R1,=H'10'                                                        
         STH   R1,0(R3)                                                         
         LA    R2,1(R2)                                                         
         LA    R3,2(R3)                                                         
         BCT   R0,EXTDEM8                                                       
         SPACE 3                                                                
         TM    NPUAOVRD,X'10'                                                   
         BNO   MOX                                                              
         MVC   OVEREL(2),=X'DD0C'                                               
         MVC   OVERDEMO,TARGET                                                  
         MVI   OVERDEMO+1,C'V'                                                  
         MVC   OVERAMNT,NPUAVOVR                                                
         MVI   OVERPREC,X'40'                                                   
         CLI   OVERDEMO,0          IS IT NAD DEMO                               
         BNE   MOX                                                              
         MVI   OVERFLG,X'80'       YES                                          
         B     MOX                                                              
         DROP  R6                                                               
         EJECT                                                                  
*              ROUTINE TO EXTRACT FROM NEW ELEMENTS                             
         SPACE 3                                                                
         USING NPUBD,R6                                                         
EXTDNEW  CLC   NPUBPER,PERIOD                                                   
         BNE   EXTDEM2                                                          
         MVC   RATING,NPUBRTG                                                   
         MVC   HUT,NPUBHUT                                                      
         MVC   SHARE,NPUBSHR                                                    
         OC    NPUBBOOK,NPUBBOOK   IF A BOOK IS IN ELEMENT                      
         BZ    *+10                                                             
         MVC   PBEL+5(2),NPUBBOOK  USE IT                                       
*        CLI   DEMSREQD,C'Y'       IF DEMOS WERE NOT REQUESTED                  
*        BE    EDN6                                                             
         LA    R2,DEMOS            LOOK FOR ANY VPH OVERRIDES                   
         LA    R3,NPUBVOVR                                                      
         LA    R1,OVERAREA                                                      
         USING OVERAREA,R1                                                      
         LA    RE,20               MAX OF 20 UNIV NAD DEMOS                     
         CLI   0(R1),0                                                          
         BE    EDN1                                                             
         LA    R1,12(R1)           BUMP PAST UNIV NADS                          
         BCT   RE,*-12                                                          
         CLI   0(R1),0             ARE WE AT THE END OF THE OVERIDES            
         BE    EDN1                GOOD                                         
         DC    H'0'                                                             
EDN1     LA    R0,6                (MAX 6)                                      
         SPACE 1                                                                
EDN2     OC    0(2,R3),0(R3)                                                    
         BZ    EDN4                                                             
         MVC   0(2,R1),=X'DD0C'    YES - SO BUILD AN OVERRIDE ELEMENT           
         MVI   OVERDEMO+1,C'V'                VPH FOR                           
         MVC   OVERDEMO+2(1),2(R2)            DEMO NUMBER                       
         MVC   OVERAMNT+2(2),0(R3)               AND AMOUNT                     
         MVI   OVERPREC,X'40'                     PRECISION                     
         CLI   0(R2),0                        IS IT NAD DEMO                    
         BE    EDN3                                                             
         MVC   OVERDEMO(1),0(R2)              YES/SET CATEGORY                  
         MVI   OVERFLG,X'80'                      NAD FLAG                      
         DROP  R1                                                               
EDN3     LA    R1,12(R1)                                                        
         SPACE 1                                                                
EDN4     LA    R2,3(R2)                                                         
         LA    R3,2(R3)                                                         
         BCT   R0,EDN2                                                          
         SPACE 1                                                                
EDN6     XC    VPHS,VPHS                                                        
         MVC   WORK(1),NPUBLNK     LINK NUMBER TO VPH ELEMENTS                  
         MVI   ELCODE,X'14'                                                     
         SPACE 1                                                                
EDN10    BAS   RE,NEXTEL                                                        
         BNE   MOX                                                              
         USING NPUCD,R6                                                         
         CLC   NPUCLNK,WORK        FIND MATCHING VPH ELEMENT                    
         BNE   EDN10                                                            
         ZIC   R1,NPUCLEN                                                       
         SH    R1,=H'5'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   VPHS(0),NPUCVPHS                                                 
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'16'        ANY NAD VPHS                                 
         BAS   RE,GETEL                                                         
         B     *+8                                                              
EDN12    BAS   RE,NEXTEL                                                        
         BNE   EDNX                                                             
         USING NPUNAD,R6                                                        
         CLC   NPUNDLNK,WORK       FIND MATCHING NAD ELEMENT                    
         BNE   EDN12                                                            
         ZIC   R5,1(R6)            LENGTH OF ELEMENT                            
         S     R5,=F'4'            .GET NUMBER OF NAD DEMOS                     
         SR    R4,R4                                                            
         D     R4,=F'5'                                                         
         LR    R4,R5               .SET IN R4                                   
         LA    R1,OVERAREA                                                      
         LA    R5,30               ROOM FOR 50 (MAX 20 NAD VPHS)                
         CLI   0(R1),0                                                          
         BE    EDN14                                                            
         LA    R1,12(R1)                                                        
         BCT   R5,*-12                                                          
         DC    H'0'                                                             
EDN14    MVC   0(2,R1),=X'DD0C'                                                 
         MVC   3(3,R1),NPUNDDEM                                                 
         MVI   6(R1),X'80'                                                      
         MVI   7(R1),X'40'                                                      
         MVC   10(2,R1),NPUNDVAL                                                
         LA    R1,12(R1)                                                        
         LA    R6,5(R6)                                                         
         BCT   R4,EDN14                                                         
         B     EDNX                                                             
*                                                                               
EDNX     B     MOX                                                              
         EJECT                                                                  
* READ FOR USER DEFINED ADJUSTMENT FACTORS - TVQ                                
GETTVQS  LA    R5,BUFF                                                          
*        LA    R5,4000(R5)                                                      
         A     R5,=F'4500'                                                      
         USING TVQBLOCK,R5                                                      
         MVI   GOTUTYPE,0                                                       
         XC    BTUEXT,BTUEXT                                                    
         XC    BTUEX1,BTUEX1                                                    
         XC    BTUEX2,BTUEX2                                                    
*                                                                               
         CLI   GDDEMO,171           TVQ CATEGORY                                
         BNE   BTUXIT               NO EXIT                                     
*                                                                               
         OC    BOOKTVQ,BOOKTVQ      ANY BOOK INPUTTED                           
         BZ    BTUXIT               NO EXIT                                     
*                                                                               
         OC    PROGNTI,PROGNTI                                                  
         BZ    BTUXIT                                                           
         LA    R6,DBLOCK                                                        
         USING DBLOCK,R6                                                        
         MVI   DBMODE,DBMDSEQ                                                   
         MVC   BTUDB,DBLOCK                                                     
         LA    R6,BTUDB                                                         
         MVC   DBFILE,=C'NAD'                                                   
         MVI   DBMODE,DBMFRST                                                   
         MVI   DBFUNCT,DBGETNTI                                                 
         L     RE,DBAREC                                                        
         USING PMKEY,RE                                                         
         MVC   DBSELPRG,PROGNTI                                                 
         MVC   DBSELSTA,NETWORK                                                 
         CLI   NETNTIST,X'40'                                                   
         BNH   *+10                                                             
         MVC   DBSELSTA,NETNTIST                                                
         MVC   DBSELSTA+4(1),NETMEDIA                                           
         CLC   DBSELSTA+3(2),=C'PN'                                             
         BNE   *+8                                                              
         MVI   DBSELSTA+3,C' '                                                  
         CLI   DBSELSTA+4,C'T'                                                  
         BNE   *+8                                                              
         MVI   DBSELSTA+4,C'N'                                                  
         CLI   DBSELSTA+4,C'S'                                                  
         BNE   *+8                                                              
         MVI   DBSELSTA+4,C'M'                                                  
         MVI   DBBTYPE,C'U'                                                     
         MVI   DBSELDAY,0                                                       
         XC    DBSELTIM(4),DBSELTIM                                             
         MVI   DBSELDUR,X'FF'      ALL DURATIONS                                
         MVI   DBSELDAY,0                                                       
         XC    DBSELTIM(4),DBSELTIM                                             
         MVI   DBPRGDUR,C'Y'                                                    
         MVI   DBBEST,C'A'                                                      
         MVI   DBSELDUR,X'FF'                                                   
*ADJUST PERIOD DATE TO A FYLL YYMMDD STRUCTURE                                  
*        MVC   WORK(2),PERIOD      PRESET TO 1ST OF MONTH                       
*        CLI   PLANPERT,C'W'                                                    
*        BE    BTUR1A                                                           
*        MVC   DUB(2),PERIOD       MOVE YEAR TO DUB                             
*        MVI   DUB+2,1                                                          
*        CLI   DUB+1,9             EXCEPT SEPTEMBER USE 19TH                    
*        BNE   *+8                 (THIS WAS GOOD FOR 1988)                     
*        MVI   DUB+2,19            (MAY NEED TO REVISE LATER)                   
*        CLI   PLANPERT,C'Q'       IF THIS IS QUARTERLY                         
*        BNE   BTUR1                  USE START OF QUARTER                      
*        ZIC   R1,PERIOD+1         PICK UP QUARTER (1-4)                        
*        BCTR  R1,0                (0-3)                                        
*        MH    R1,=H'3'            (0,3,6,9)                                    
*        LA    R1,1(R1)            (1,4,7,10)                                   
*        STC   R1,DUB+1                                                         
*        CLI   DUB+1,10            IF 4TH QUARTER                               
*        BNE   BTUR1               LISA                                         
*        MVI   DUB+1,11            SAYS USE NOV 1                               
*        MVI   DUB+2,1                                                          
*        SPACE 1                                                                
*  BTUR1    GOTO1 DATCON,DMCB,(3,DUB),(2,WORK)                                  
*  BTUR1A   GOTO1 DATCON,DMCB,(2,WORK),(0,WORK+3)                               
         GOTO1 DATCON,DMCB,(2,BOOKTVQ),(0,WORK+3)                               
         GOTO1 NETWEEK,DMCB,WORK+3,GETDAY,ADDAY                                 
         MVC   DBSELBK(1),4(R1)                                                 
         MVC   DBSELBK+1(1),8(R1)                                               
*                                                                               
         LA    RE,TVQDATES         YES - CONVET TO TVQ BOOK                     
BTUR2    CLI   0(RE),X'FF'         NOT FOUND - ALLOW A MISS                     
         BE    BTUR4                                                            
         CLC   DBSELBK,2(RE)       CURRENT OUTSIDE RANGE                        
         BH    BTUR3               GET NEXT                                     
         CLC   DBSELBK,0(RE)       TOTALLY OUTSIDE RANGE - ALLOW MISS           
         BL    BTUR4                                                            
         MVC   DBSELBK(2),4(RE)    FOUND THE EQUATE - USE IT                    
         B     BTUR4                                                            
BTUR3    LA    RE,6(RE)                                                         
         B     BTUR2                                                            
*                                                                               
BTUR4    XC    DBAQUART,DBAQUART                                                
         XC    BTUAQ,BTUAQ                                                      
         LA    RE,BTUIO                                                         
         ST    RE,DBAREC                                                        
         L     RF,=F'1000'                                                      
         XCEF                                                                   
         DROP  R6,RE                                                            
*                                                                               
         GOTO1 DEMAND,BTUDMCB,BTUDB,BTUHK                                       
*                                                                               
         LA    RF,BTUDB                                                         
         USING DBLOCK,RF                                                        
         OC    DBAQUART(4),DBAQUART                                             
         BNZ   BTUXIT                                                           
         CLI   GOTUTYPE,1                                                       
         BNE   BTUXIT                                                           
         MVC   DBAQUART(4),BTUAQ                                                
         B     BTUXIT                                                           
BTUHK    LA    RF,BTUDB                                                         
         MVC   BTUAQ,8(RF)                                                      
         MVI   GOTUTYPE,1                                                       
         BR    RE                                                               
BTUXIT   B     MOX                                                              
         DROP  RF                                                               
         EJECT                                                                  
*              EXTRACT PERIODS FOR PROGRAM                                      
         SPACE 3                                                                
EXTPER   XC    PLANPLST,PLANPLST                                                
         CLI   PLANPERT,C'W'                                                    
         BE    EXTWEEK                                                          
         MVI   PLANNPER,13                                                      
         LA    R3,SKELMLST                                                      
         CLI   PLANPERT,C'M'                                                    
         BE    EXTQM                                                            
         MVI   PLANNPER,4                                                       
         LA    R3,SKELQLST                                                      
         SPACE 1                                                                
EXTQM    LA    R2,PLANPLST                                                      
         ZIC   R0,PLANNPER                                                      
         SPACE 1                                                                
EXTQM2   MVC   0(4,R2),0(R3)       MONTHS AND QUARTERS HERE                     
         ZIC   R1,PLANYEAR                                                      
         CLI   0(R2),1                                                          
         BNE   *+6                                                              
         BCTR  R1,0                                                             
         STC   R1,0(R2)            EITHER PLAN YEAR OR PREVIOUS                 
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R0,EXTQM2                                                        
         B     MOX                                                              
         SPACE 1                                                                
*              YEAR/PERIOD#/REL Q/REL M                                         
SKELQLST DC    AL1(1,4,0,0)        QUARTERS                                     
         DC    AL1(2,1,1,4)                                                     
         DC    AL1(2,2,2,7)                                                     
         DC    AL1(2,3,3,10)                                                    
SKELMLST DC    AL1(1,9,0,0)        MONTHS                                       
         DC    AL1(1,10,0,1)                                                    
         DC    AL1(1,11,0,2)                                                    
         DC    AL1(1,12,0,3)                                                    
         DC    AL1(2,1,1,4)                                                     
         DC    AL1(2,2,1,5)                                                     
         DC    AL1(2,3,1,6)                                                     
         DC    AL1(2,4,2,7)                                                     
         DC    AL1(2,5,2,8)                                                     
         DC    AL1(2,6,2,9)                                                     
         DC    AL1(2,7,3,10)                                                    
         DC    AL1(2,8,3,11)                                                    
         DC    AL1(2,9,3,12)                                                    
         EJECT                                                                  
*              EXTRACT WEEKS FOR PROGRAM                                        
         SPACE 1                                                                
EXTWEEK  MVI   PLANNPER,16         WEEKS - UP TO 16                             
         ZIC   R1,PROGPERQ         GET END OF QUARTER FROM QUARTER NO.          
         SLL   R1,2                                                             
         LA    R1,QENDDATE(R1)                                                  
         MVC   WORK+2(4),0(R1)                MMDD FROM TABLE                   
         ZIC   R2,PLANYEAR         PLAN YEAR                                    
         CLI   PROGPERQ,0                                                       
         BNE   *+6                                                              
         BCTR  R2,0                OR PREVIOUS FOR FOURTH QUARTER               
         EDIT  (R2),(2,WORK),WRK=DMCB,FILL=0                                    
         SPACE 1                                                                
         GOTO1 GETDAY,DMCB,WORK,WORK+6       FIND PREVIOUS END OF WEEK          
         ZIC   R0,DMCB             (END OF QUARTER DAY IN R0)                   
         ZIC   R1,ZEROPROF+8       PICK UP PROFILE START OF WEEK                
         LTR   R1,R1                                                            
         BNZ   *+8                                                              
         LA    R1,1                (DEFAULT IS MONDAY)                          
         BCT   R1,*+8              BACK UP TO END OF WEEK                       
         LA    R1,7                MONDAY BACKS UP TO SUNDAY                    
         SR    R1,R0                                                            
         BZ    WKD2                                                             
         BM    *+8                                                              
WKD1     SH    R1,=H'7'                                                         
         CLI   N0PROF+3,C'C'       IF MEDIA IS SET UP FOR CALENDAR              
         BNE   *+8                    POP IN ANOTHER WEEK UNLESS                
         AH    R1,=H'7'               QUARTER ENDED ON 'SUNDAY'                 
         ST    R1,DMCB+8                                                        
*                                                                               
         CLC   WORK(4),=C'9610'    ,,IF 3D QRT 96                               
         BNE   WKD1B                                                            
         CLI   N0PROF+3,C'C'       ,,IF CALENDAR                                
         BNE   WKD1B                                                            
         A     R1,=F'-7'           ,,DROP 1 WEEK                                
         ST    R1,DMCB+8                                                        
*                                                                               
WKD1B    CLC   WORK(6),=C'020331'  ,,IF 1S QRT 02                               
         BNE   WKD1E                                                            
         CLI   N0PROF+3,C'C'       ,,IF CALANDER                                
         BNE   WKD1E                                                            
         A     R1,=F'7'            ,,ADD ONE WEEK                               
         ST    R1,DMCB+8                                                        
*                                                                               
WKD1E    CLC   WORK(6),=C'020630'  ,,IF 2N QRT 02                               
         BNE   WKD1J                                                            
         CLI   N0PROF+3,C'C'       ,,IF CALANDER                                
         BNE   WKD1J                                                            
         A     R1,=F'7'            ,,ADD ONE WEEK                               
         ST    R1,DMCB+8                                                        
*                                                                               
WKD2B    GOTO1 ADDAY,DMCB,WORK,WORK+6                                           
*                                                                               
*                                                                               
WKD1J    GOTO1 ADDAY,DMCB,WORK,WORK+6                                           
         MVC   WORK(6),WORK+6                                                   
         SPACE 1                                                                
*-- NEXT FOUR LINES ARE NEEDED TO CORRECT A FLAW IN THE 1995                    
*-- YEAR DATE CALCULATIONS, DEALING WITH THE THIRD QUARTER.                     
WKD2     CLC   WORK(6),=CL6'951001'                                             
         BNE   *+12                                                             
         CLI   N0PROF+3,C'B'       IF WE ARE USING BROADCAST                    
         BE    WKD1                   ADD IN ANOTHER WEEK                       
*-- NEXT FOUR LINES ARE NEEDED TO CORRECT A FLAW IN THE 2002                    
*-- YEAR DATE CALCULATIONS, DEALING WITH THE SECOND QUARTER.                    
         CLC   WORK(6),=CL6'020331'                                             
         BNE   *+12                                                             
         CLI   N0PROF+3,C'C'       IF WE ARE USING BROADCAST                    
         BE    WKD1                   ADD IN ANOTHER WEEK                       
*-- NEXT FOUR LINES ARE NEEDED TO CORRECT A FLAW IN THE 2002                    
*-- YEAR DATE CALCULATIONS, DEALING WITH THE THIRD QUARTER.                     
         CLC   WORK(6),=CL6'020630'                                             
         BNE   *+12                                                             
         CLI   N0PROF+3,C'C'       IF WE ARE USING BROADCAST                    
         BE    WKD1                   ADD IN ANOTHER WEEK                       
*                                                                               
         L     R1,=F'-111'         BACK UP 16 WEEKS TO FIRST MONDAY             
         ST    R1,DMCB+8                                                        
         GOTO1 ADDAY,DMCB,WORK,WORK+6                                           
         LA    R2,PLANPLST                                                      
         LA    R0,16                                                            
         SPACE 1                                                                
WKD4     MVC   WORK(6),WORK+6      THEN GENERATE 16 MONDAYS                     
         GOTO1 DATCON,DMCB,(0,WORK),(2,0(R2))                                   
         MVC   2(1,R2),PROGPERQ    QUARTER NUMBER (0-3)                         
         GOTO1 ADDAY,DMCB,WORK,WORK+6,F'6'                                      
         MVC   WORK(6),WORK+6      NOW GOT TO SUNDAY                            
         GOTO1 DATCON,DMCB,(0,WORK),(3,WORK+6)                                  
         ZIC   R1,WORK+7           PICK UP MONTH NUMBER OF END 1-12             
         LA    R1,3(R1)            (4-15)                                       
         CLI   PROGPERQ,0                                                       
         BNE   *+8                                                              
         SH    R1,=H'12'           ADJUST FOURTH QUARTER                        
         STC   R1,3(R2)                                                         
         CLI   3(R2),15            IF MONTH IS STILL 15                         
         BNE   *+8                                                              
         MVI   3(R2),4             MAKE THIS JANUARY AS USER HAD                
*                                  INCLUDED DECEMBER UNITS IN 1ST Q             
         GOTO1 ADDAY,DMCB,WORK,WORK+6,F'1'                                      
         LA    R2,4(R2)                                                         
         BCT   R0,WKD4                                                          
         B     MOX                                                              
         SPACE 1                                                                
QENDDATE DC    C'1231033106301001'    FOUR MMDD PAIRS                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              TABLES OF RECORDS ACTIONS AND COMBINATIONS                       
         SPACE 3                                                                
RECACTS  DS    0D                                                               
         SPACE 1                                                                
* X'01' ENTRIES ARE AVAILABLE REPORTS                                           
*                                                                               
* CL8 EXPANDED REPORT NAME                                                      
* CL1 REPORT NUMBER (01 IS RESERVED)                                            
* CL1 PHASE NUMBER FOR DATA DICTIONARY - LEAVE AS ZERO FOR NOW                  
* CL1 PHASE NUMBER FOR HELP SCREEN - LEAVE AS ZERO FOR NOW                      
         SPACE 1                                                                
         DC    X'01',C'PLAN    ',AL1(01),X'0000'                                
         DC    X'01',C'PROGRAM ',AL1(02),X'0000'                                
         DC    X'04',C'REQUEST ',AL1(04),X'0000'                                
         DC    X'04',C'REPORT  ',AL1(04),X'0000'                                
         DC    X'04',C'PRB1    ',AL1(05),X'0000'                                
         DC    X'04',C'UNIT    ',AL1(05),X'0000'                                
         DC    X'04',C'PRB2    ',AL1(06),X'0000'                                
         DC    X'04',C'DEMO    ',AL1(06),X'0000'                                
         DC    X'04',C'PRB3    ',AL1(07),X'0000'                                
         DC    X'04',C'QUART   ',AL1(07),X'0000'                                
         DC    X'04',C'PRB4    ',AL1(08),X'0000'                                
         DC    X'04',C'EVAL    ',AL1(08),X'0000'                                
         DC    X'04',C'PWEEK   ',AL1(09),X'0000'                                
         DC    X'04',C'PRB6    ',AL1(10),X'0000'                                
         DC    X'04',C'NET     ',AL1(10),X'0000'                                
         DC    X'04',C'PRB7    ',AL1(11),X'0000'                                
         DC    X'01',C'QSET    ',AL1(12),X'0000'                                
         DC    X'01',C'QPROG   ',AL1(13),X'0000'                                
*        DC    X'01',C'QPLAN   ',AL1(14),X'0000'                                
         SPACE 2                                                                
* X'02' ENTRIES ARE AVAILABLE ACTIONS                                           
*                                                                               
* CL8 EXPANDED ACTION NAME                                                      
* CL1 ACTION NUMBER                                                             
* CL1 ACTION EQUATE                                                             
* CL1 SPARE                                                                     
         SPACE 1                                                                
         DC    X'02',C'ADD     ',AL1(01,01,00)                                  
         DC    X'02',C'CHANGE  ',AL1(02,01,00)                                  
         DC    X'02',C'DISPLAY ',AL1(03,01,00)                                  
         DC    X'02',C'DELETE  ',AL1(04,01,00)                                  
         DC    X'02',C'SELECT  ',AL1(05,01,00)                                  
         DC    X'02',C'RESTORE ',AL1(06,01,00)                                  
         DC    X'02',C'LIST    ',AL1(10,10,00)                                  
         DC    X'02',C'CLIST   ',AL1(10,11,00)                                  
         DC    X'02',C'SLIST   ',AL1(10,11,00)                                  
         DC    X'02',C'HLIST   ',AL1(10,11,00)                                  
         DC    X'02',C'ULIST   ',AL1(10,11,00)                                  
         DC    X'02',C'RLIST   ',AL1(10,11,00)                                  
         DC    X'02',C'VLIST   ',AL1(10,11,00)                                  
         DC    X'02',C'REPORT  ',AL1(12,12,00)                                  
         DC    X'02',C'VALUE   ',AL1(07,14,00)                                  
         DC    X'02',C'REVALUE ',AL1(07,14,00)                                  
         DC    X'02',C'PVALUE  ',AL1(07,14,00)                                  
         DC    X'02',C'COPY    ',AL1(07,16,00)                                  
         DC    X'02',C'TRANSFER',AL1(07,18,00)                                  
         DC    X'02',C'COMPARE ',AL1(07,20,00)                                  
         DC    X'02',C'OVERRIDE',AL1(07,24,00)                                  
         DC    X'02',C'COST    ',AL1(07,26,00)                                  
         DC    X'02',C'CPM     ',AL1(08,08,00)                                  
         DC    X'02',C'STADD   ',AL1(01,28,00)                                  
         DC    X'02',C'WRITER  ',AL1(22,22,00)                                  
         DC    X'02',C'HELP    ',AL1(00,00,00)                                  
         SPACE 2                                                                
* DIRECTORY OF PHASES FOR SELECTED RECORD/ACTION                                
         SPACE 1                                                                
* X'03' ENTRIES ARE OK REC/ACT COMBOS                                           
* CL1 RECORD NUMBER                                                             
* CL1 ACTION EQUATE                                                             
* CL1 PHASE NUMBER FOR SCREEN                                                   
* CL1 PHASE NUMBER FOR EDIT                                                     
* CL1 PHASE NUMBER FOR SPECS                                                    
* CL1 PHASE NUMBER FOR REPORT                                                   
* CL1 WHEN OK BITS 80=SCREEN 40=NOW 20=SOON 10=OV 08=DDS                        
* CL2 CODE FOR REPORTS                                                          
* CL2 CODE FOR EOD HANDLING                                                     
         SPACE 1                                                                
*                                 SC  SP  OK                                    
*                                   OV  RP                                      
*                                                                               
*  PLAN MAINT WAS CHANGED FROM F101 TO FA0A TO TEST QPLAN ON SCRIPT             
*                                                                               
         DC    X'03',AL1(01,01),X'F101000180',C'PLUR'  PLAN     MAINT           
         DC    X'03',AL1(01,10),X'F202000280',C'PLUR'  PLAN     LIST            
         DC    X'03',AL1(01,12),X'F303000378',C'PLUR'  PLAN     REPORT          
         DC    X'03',AL1(01,14),X'F404000480',C'PLUR'  PLAN     VALUE           
         DC    X'03',AL1(01,16),X'F505000580',C'PLUR'  PLAN     COPY            
         DC    X'03',AL1(01,18),X'F606000680',C'PLUR'  PLAN     TRANS           
         DC    X'03',AL1(01,20),X'F707000780',C'PLUR'  PLAN     COMPARE         
         DC    X'03',AL1(01,08),X'E615001580',C'PLUR'  PLAN     CPM             
         DC    X'03',AL1(02,01),X'E111001180',C'PRUR'  PROGRAM  MAINT           
         DC    X'03',AL1(02,28),X'E111001181',C'PRUR'  PROGRAM  MAINT           
         DC    X'03',AL1(02,10),X'E212001280',C'PRUR'  PROGRAM  LIST            
         DC    X'03',AL1(02,24),X'E313001380',C'PRUR'  PROGRAM  OVERIDE         
         DC    X'03',AL1(02,26),X'E716001180',C'PRUR'  PROGRAM  COST            
         DC    X'03',AL1(02,11),X'E817001780',C'PRUR'  PROGRAM  ULIST           
         DC    X'03',AL1(04,12),X'D121002178',C'PSUR'  REQUEST  REPORT          
         DC    X'03',AL1(04,22),X'D222002238',C'PSUW'  REPORT   WRITER          
         DC    X'03',AL1(05,12),X'D323002378',C'B1UW'  PRB1     REPORT          
         DC    X'03',AL1(06,12),X'D424002478',C'B2UW'  PRB2     REPORT          
         DC    X'03',AL1(07,12),X'D525002578',C'B3UW'  PRB3     REPORT          
         DC    X'03',AL1(08,12),X'D626002678',C'B4UW'  PRB4     REPORT          
         DC    X'03',AL1(09,12),X'D727002778',C'PWUW'  PWEEK    REPORT          
         DC    X'03',AL1(10,12),X'D828002878',C'B6UW'  PRB6     REPORT          
         DC    X'03',AL1(11,12),X'D929002978',C'B7UW'  PRB7     REPORT          
         DC    X'03',AL1(12,01),X'E414001480',C'QSUR'  QSET     MAINT           
         DC    X'03',AL1(12,10),X'E514001480',C'QSUR'  QSET     LIST            
         DC    X'03',AL1(13,01),X'F808000880',C'PRCB'  CABLE    MAINT           
         DC    X'03',AL1(13,10),X'F908000880',C'PRCB'  CABLE    LIST            
         DC    X'03',AL1(14,01),X'FA0A000180',C'PLUR'  QPLAN    MAINT           
         DC    X'FF'                                                            
         SPACE 3                                                                
*              PHASE USED UP BY SYSTEM SO FAR                                   
         SPACE 1                                                                
*              X0 X1 X2 X3 X4 X5 X6 X7 X8 X9 XA XB XC XD XE XF                  
*                                                                               
*        0X     Y  Y  Y  Y  Y  Y  Y  Y  Y     Y                                 
*        1X        Y  Y  Y  Y  Y  Y  Y                                          
*        2X        Y  Y  Y  Y  Y  Y  Y  Y  Y                                    
*        FX        Y  Y  Y  Y  Y  Y  Y  Y  Y  Y              Y                  
*        EX        Y  Y  Y  Y  Y  Y  Y  Y                                       
*        DX        Y  Y  Y  Y  Y  Y  Y  Y  Y                                    
         SPACE 3                                                                
         PRINT GEN                                                              
       ++INCLUDE DETVQDATE                                                      
         PRINT NOGEN                                                            
*TVQDATES DC    AL1(97,33),AL1(97,39),AL1(97,09)  08/11/97-09/28/97             
*         DC    AL1(97,40),AL1(97,42),AL1(97,10)  09/29/97-10/19/97             
*         DC    AL1(97,43),AL1(97,47),AL1(97,11)  10/20/97-11/23/97             
*         DC    AL1(97,48),AL1(98,02),AL1(98,01)  11/24/97-01/11/98             
*         DC    AL1(98,03),AL1(98,08),AL1(98,02)  01/12/98-02/22/98             
*         DC    AL1(98,09),AL1(98,14),AL1(98,03)  02/23/98-04/05/98             
*         DC    AL1(98,15),AL1(98,24),AL1(98,05)  04/06/98-06/14/98             
*         DC    AL1(98,25),AL1(98,37),AL1(98,07)  06/15/98-08/09/98             
*         DC    AL1(255,255,255,255,255,255)                                    
         EJECT                                                                  
       ++INCLUDE NEPUPALLN                                                      
         EJECT                                                                  
*              CTGENFILE                                                        
*              SPGENAGY                                                         
*              SPGENCLT                                                         
*              SPGENSTA                                                         
*              SPGENPROG                                                        
*              DDCOMFACS                                                        
*              FAFACTS                                                          
*              FATIOB                                                           
*              NEGETHUTD                                                        
*              NEGETNUND                                                        
         PRINT ON                                                               
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE SPGENAGY                                                       
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE SPGENPROG                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FATIOB                                                         
       ++INCLUDE NEGETHUTD                                                      
       ++INCLUDE NEGETNUND                                                      
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE DEDEMFILE                                                      
       ++INCLUDE FASECRETD                                                      
       ++INCLUDE DDOFFICED                                                      
         PRINT ON                                                               
         SPACE 3                                                                
TVQBLOCK DSECT                                                                  
GOTUTYPE DS    CL1                                                              
*                                                                               
         DS    0F                                                               
BTUEXT   DS    CL4                                                              
BTUEX1   DS    A(0)                                                             
BTUEX2   DS    A(0)                                                             
*                                                                               
BTUAQ    DS    F                                                                
BTUDMCB  DS    10F                                                              
BTUDB    DS    XL256                                                            
         DS    CL4                                                              
BTUIO    DS    1000C                                                            
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'227NEPUP00   05/01/02'                                      
         END                                                                    
