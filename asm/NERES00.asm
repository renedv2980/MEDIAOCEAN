*          DATA SET NERES00    AT LEVEL 150 AS OF 06/09/17                      
*PHASE T32100A,*                                                                
*INCLUDE MILEDIT                                                                
*INCLUDE NETBROWN                                                               
*INCLUDE NETUNWK                                                                
*INCLUDE KHDUMMY                                                                
T32100   TITLE '-   NETPAK RESEARCH CONTROLLER'                                 
T32100   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LENWORK,T32100**,RA,RR=R2,CLEAR=YES                              
         USING GEND,RC                                                          
         SPACE 1                                                                
         USING SPOOLD,R8                                                        
         USING SYSD,R9             SYSTEM SPECIFIC WORK                         
         SPACE 1                                                                
         ST    R2,RELO                                                          
         LR    R8,RC               R8=A(SPOOLD)                                 
         LA    RC,SPOOLEND         RC=A(GENCON STORAGE)                         
         LA    R9,IO               START OF IO 1 + TOTAL IO AREA LEN            
         AH    R9,=Y(IOLEN)        R9=A(RESRCH SYSTEM WORKNG STORAGE)           
         SPACE 1                                                                
         ST    R1,SYSPARMS         THIS CODE FOR CURSOR POSITIONING             
         L     R0,0(R1)                                                         
         ST    R0,ATIOB                                                         
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
*                                                                               
*                                                                               
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
         MVC   DUMPDBA,=C'**DBA***'                                             
         MVC   DUMPDBB,=C'**DBB***'                                             
         MVC   DUMPHUT,=C'**HUT***'                                             
         MVC   DUMPEVN,=C'**EVN***'                                             
         MVC   DUMPFILT,=C'**FILT**'                                            
         MVC   DUMPBUFF,=C'**BUFF**'                                            
         MVC   DUMPHEAD,=C'**HEAD**'                                            
         EJECT                                                                  
*              SET SYSTEM DEPENDENT VALUES                                      
         SPACE 3                                                                
*                                                                               
         MVI   SYSTEM,C'N'         NETWORK                                      
         MVI   GETMSYS,5           GENCON MESSAGES                              
         SPACE 1                                                                
         MVI   MAXIOS,NIOS         USES 2 I/O AREAS                             
         MVC   SIZEIO,=AL4(LIOS)   EACH I/O IS 1000 BYTES                       
         MVC   SYSDUMMY,VDUMMY     END OF SYSTEM BASE                           
         MVC   GETUSER,VGETAGY     ROUTINE TO GET AGY NAME AND ADDRESS          
         SPACE 1                                                                
         MVC   LKEY,=H'13'          DETAILS OF DIRECTORY AND KEY                
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'24'                                                  
         MVC   SYSFIL,=C'SPTFIL  '                                              
         MVC   SYSDIR,=C'SPTDIR  '                                              
         MVC   REQFILE,=C'REQUEST '                                             
         SPACE 1                                                                
         MVC   LWORK,=Y(LENWORK)        SET WORK AREA LENGTH                    
         MVC   RCPROG(2),=C'NE'         PREFIX FOR REPORT NO.                   
         MVC   SYSPHASE,=X'D9032100'    PRESET FOR SYSTEM CALLOVS               
         L     R1,=A(RECACTS)           RECORD/ACTION DIRECTORY                 
         A     R1,RELO                                                          
         ST    R1,ARECACT                                                       
         SPACE 1                                                                
*                                  SET UP CERTAIN ADDRESSES                     
         L     R1,SYSPARMS                                                      
         LM    R2,R4,8(R1)         A(SYSLIST) A(TIA) A(COMFACS)                 
         ST    R4,ACOMFACS                                                      
         ST    R4,RCCOMFAC                                                      
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
         SPACE 1                                                                
         L     R1,=V(MILEDIT)      RELOCATE LINKED MODULES                      
         A     R1,RELO                                                          
         ST    R1,MILEDIT                                                       
         L     R1,=V(NETBROWN)                                                  
         A     R1,RELO                                                          
         ST    R1,NETBROWN                                                      
         L     R1,=V(NETUNWK)                                                   
         A     R1,RELO                                                          
         ST    R1,NETUNWK                                                       
         B     XIT                                                              
         DROP  R4                                                               
         SPACE 2                                                                
IOLEN    EQU   NIOS*(LIOS+8)                                                    
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
VBRANCH  B     VALSRC                                                           
         B     VALBOOK                                                          
         B     VALDEM                                                           
         B     VALNET                                                           
         B     VALDYTM                                                          
         B     VALTIM                                                           
         B     VALDLVL                                                          
         B     GETAGY                                                           
         B     ERRXIT                                                           
         B     VALTITL                                                          
         B     TITOUT                                                           
         B     RESHEAD                                                          
         B     VALDAY                                                           
         B     VALFILT                                                          
         B     CHEFILT                                                          
         B     PROGHUT                                                          
         B     VALSEND                                                          
         B     ADJSEL                                                           
         B     DISPNET                                                          
         B     CURSERR                                                          
         B     EXPBOOK                                                          
         SPACE 1                                                                
VCOUNT   EQU   (*-VBRANCH)/4                                                    
         EJECT                                                                  
*              VALIDATE SOURCE                                                  
         SPACE 3                                                                
*              INPUT               OPTION X'80' PROGRAMS ALLOWED                
*                                         X'40' NTI,T ALLOWED                   
*                                         X'20' NAD ALLOWED                     
*              OUTPUTS             FIELDS IN MAIN DBLOCK....                    
*                                  DBCOMFACS DBAREC DBAQUART                    
*                                  DBSELAGY DBSELMED DBSELSRC                   
         SPACE 1                                                                
VALSRC   GOTO1 ANY                                                              
         ST    R2,ARESSRCE                                                      
         LA    R4,DBLOCKA                                                       
         USING DBLOCKD,R4                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBSELAGY,AGENCY     FOR LOCKOUTS                                 
         MVC   DBAUTH,TWAAUTH                                                   
         LA    R1,IO                                                            
         ST    R1,DBAREC                                                        
         SPACE 1                                                                
         MVI   DBSELSRC,C'N'       NTI VALIDATION                               
         MVI   DBSELMED,C'N'                                                    
         MVI   DBFUNCT,DBGETDEM                                                 
         MVC   DBFILE,=C'NHI'                                                   
         CLC   8(5,R2),=C'NHI,T'                                                
         BE    SRCEND                                                           
         MVC   DBFILE,=C'NTI'                                                   
         CLC   8(5,R2),=C'NTI,T'   (NTI,T FOR TIME PERIOD DATA)                 
         BE    VALSRC1                                                          
         CLC   8(5,R2),=C'WB1,T'  TCAR SUPPORT                                  
         BNE   VALSRC2                                                          
*                                                                               
VALSRC1  TM    OPTION,X'40'        WAS PROGRAM AUTHORIZED?                      
         BO    SRCEND                                                           
         B     BADSRCE                                                          
         SPACE 1                                                                
VALSRC2  MVI   DBFUNCT,DBGETNTI                                                 
         CLC   8(3,R2),=C'NTI'                                                  
         BE    SRCEND                                                           
         CLC   8(3,R2),=C'WB1'    TCAR SUPPORT                                  
         BE    SRCEND                                                           
         SPACE 1                                                                
         TM    OPTION,X'20'        IS PROGRAM AUTHORIZED FOR NAD?               
         BNO   VALSRC4                                                          
         MVC   DBFILE,=C'NAD'      NAD VALIDATION                               
         MVI   NADSECT,C'T'                                                     
         CLC   8(5,R2),=C'NAD,T'                                                
         BE    SRCEND                                                           
         MVI   NADSECT,C'D'                                                     
         CLC   8(5,R2),=C'NAD,D'                                                
         BE    SRCEND                                                           
         MVI   NADSECT,C'P'                                                     
         CLC   8(3,R2),=C'NAD'                                                  
         BE    SRCEND                                                           
         SPACE 1                                                                
VALSRC4  TM    OPTION,X'80'        IS PROGRAM AUTHORIZED FOR EVN?               
         BNO   BADSRCE                                                          
         OI    6(R2),X'80'         EVN - NETWORK PROGRAM FILE                   
         MVC   DBFILE,=C'EVN'      EXPRESSION IS P,START(,END)                  
         MVI   DBFUNCT,DBGETDEM                                                 
         CLI   8(R2),C'P'                                                       
         BNE   BADSRCE                                                          
         SPACE 1                                                                
         GOTO1 SCANNER,PARAS,(R2),(3,BLOCK)                                     
         XC    PSTART,PSTART                                                    
         MVC   PEND,=X'FFFF'                                                    
         LA    R1,PIO                                                           
         ST    R1,DBAREC                                                        
         LA    R1,PIO+22                                                        
         ST    R1,DBAQUART                                                      
         MVI   DBSELMED,C'V'                                                    
         CLI   PARAS+4,1                                                        
         BE    SRCEND                                                           
         SPACE 1                                                                
         MVI   FIELDERR,2          VALIDATE START                               
         GOTO1 DATVAL,DMCB,(0,BLOCK+44),DUB                                     
         CLI   DMCB+3,0                                                         
         BE    BADDATE                                                          
         SPACE 1                                                                
         MVI   FIELDERR,3          VALIDATE END                                 
         CLI   BLOCK+32,8                                                       
         BH    BADDATE                                                          
         GOTO1 DATCON,DMCB,(0,DUB),(2,PSTART)                                   
         CLI   PARAS+4,2                                                        
         BE    SRCEND                                                           
         GOTO1 DATVAL,DMCB,(0,BLOCK+76),DUB                                     
         CLI   DMCB+3,0                                                         
         BE    BADDATE                                                          
         GOTO1 DATCON,DMCB,(0,DUB),(2,PEND)                                     
         CLC   PSTART,PEND                                                      
         BH    EBEFST                                                           
         SPACE 1                                                                
SRCEND   MVC   SVFILE,DBFILE       SAVE SOME DBLOCK VALUES                      
         MVC   SVSOURCE,DBSELSRC                                                
         MVC   SVMEDIA,DBSELMED                                                 
         B     XIT                                                              
         SPACE 1                                                                
BADSRCE  MVC   CONHEAD(L'INVSRC),INVSRC    INVALID SOURCE                       
         B     MYEND                                                            
         EJECT                                                                  
*              VALIDATE START/END FOR SOURCE OF PROGRAMS                        
         SPACE 3                                                                
VALSEND  L     R1,ARESSRCE         ONLY NEEDED FOR SOURCE                       
         CLI   8(R1),C'P'          OF P                                         
         BNE   XIT                                                              
         MVI   FIELDERR,1                                                       
         OC    PSTART,PSTART       DO START FIRST                               
         BNZ   VALSEND2                                                         
         GOTO1 ANY                                                              
         GOTO1 DATVAL,DMCB,(0,8(R2)),DUB                                        
         CLI   DMCB+3,0                                                         
         BE    BADDATE                                                          
         GOTO1 DATCON,DMCB,(0,DUB),(2,PSTART)                                   
         SPACE 1                                                                
VALSEND2 BAS   RE,BUMP                                                          
         BAS   RE,BUMP                                                          
         CLC   PEND(2),=X'FFFF'    THEN THE END DATE                            
         BNE   VALSEND4                                                         
         GOTO1 ANY                                                              
         GOTO1 DATVAL,DMCB,(0,8(R2)),DUB                                        
         CLI   DMCB+3,0                                                         
         BE    BADDATE                                                          
         GOTO1 DATCON,DMCB,(0,DUB),(2,PEND)                                     
         SPACE 1                                                                
VALSEND4 CLC   PSTART,PEND         START V END                                  
         BNH   XIT                                                              
         SPACE 1                                                                
EBEFST   L     RE,=A(ENDERR)               RAN OUT OF USING RANGE               
         A     RE,RELO                                                          
         MVC   CONHEAD(L'ENDERR),0(RE)     END BEFORE START                     
         B     MYEND                                                            
         SPACE 1                                                                
BADDATE  L     RE,=A(INVDAT)                                                    
         A     RE,RELO                                                          
         MVC   CONHEAD(L'INVDAT),0(RE)     BAD DATE                             
         B     MYCURSOR                                                         
         DROP  R4                                                               
         EJECT                                                                  
*              VALIDATION OF BOOKS                                              
         SPACE 3                                                                
VALBOOK  GOTO1 ANY                 BOOK                                         
         NI    BOOKOPT,X'1F'       CLEAR X'E0' BITS                             
         ST    R2,ARESBOOK                                                      
         MVI   NUMBOOKS,0                                                       
         LA    R6,DBLOCKA                                                       
         USING DBLOCK,R6                                                        
         MVC   DMCB+8(4),=C',=,-'    (MAY HAVE BOOK-BOOK RANGE)                 
         GOTO1 SCANNER,DMCB,(R2),(10,BLOCK)                                     
         ZIC   R0,4(R1)                                                         
         LA    R3,BLOCK                                                         
         MVI   FIELDERR,1                                                       
         LTR   R0,R0                                                            
         BZ    BADBOOK                                                          
         XC    BOOKS,BOOKS                                                      
         LA    R4,BOOKS                                                         
         CH    R0,=H'1'            IF ONLY 1 EXPRESSION                         
         BH    BOOK1                                                            
         CLI   1(R3),0                                                          
         BNE   BOOK2                                                            
         SPACE 1                                                                
BOOK1    BAS   RE,SOFTBOOK         CHECK FOR SOFT BOOK EXPRESSIONS              
         BE    XIT                                                              
         SPACE 1                                                                
BOOK2    CLC   12(3,R3),=C'ALT'    ALTERNATING OPTION                           
         BNE   BOOK4                                                            
         MVI   ALTOPT,C'Y'                                                      
         SH    R4,=H'3'                                                         
         B     BOOKNEXT                                                         
         SPACE 1                                                                
BOOK4    XC    DMCB(12),DMCB                                                    
         ZIC   RF,0(R3)            LENGTH OF (FIRST) PHRASE                     
         ST    RF,DMCB+4                                                        
         GOTO1 ONEBOOK,DMCB,12(R3),,(R4)                                        
         BNE   BADBOOK                                                          
         SPACE 1                                                                
         CLI   1(R3),0             IS THERE A RANGE OF BOOKS                    
         BE    BOOKNEXT                                                         
         OI    0(R4),X'01'                                                      
         LA    R4,3(R4)                                                         
         ZIC   RF,1(R3)            LENGTH OF SECOND PHRASE                      
         ST    RF,DMCB+4                                                        
         GOTO1 ONEBOOK,DMCB,22(R3),,(R4)                                        
         BNE   BADBOOK                                                          
*                                                                               
         LR    RE,R4                                                            
         SH    RE,=H'3'                                                         
         CLC   1(1,RE),4(RE)       CHECK END YEAR PAST START YEAR               
         BL    BOOKNEXT            YES - OK                                     
         CLI   2(RE),48            TEST BLACK WEEK IN START                     
         BH    BOOK6                                                            
         CLI   5(RE),48            TEST BLACK WEEK IN END                       
         BNH   BOOK9                                                            
*                                  TAKE CARE OF BLACK WEEK RANGES               
BOOK6    MVC   DUB,0(RE)                                                        
         LA    RE,DUB                                                           
         CLI   DUB+1,89            1989 IS A 53 WEEK YEAR                       
         BE    BOOK8                                                            
         CLI   DUB+1,84            1984 IS A 53 WEEK YEAR                       
         BE    BOOK7                                                            
         TR    DUB+2(1),BLKLIST    REPLACE START WEEK                           
         TR    DUB+5(1),BLKLIST    REPLACE END WEEK                             
         B     BOOK9                                                            
BOOK7    TR    DUB+2(1),BLK1984    REPLACE START WEEK                           
         TR    DUB+5(1),BLK1984    REPLACE END WEEK                             
         B     BOOK9                                                            
*                                                                               
BOOK8    TR    DUB+2(1),BLK1989    REPLACE START WEEK                           
         TR    DUB+5(1),BLK1989    REPLACE END WEEK                             
*                                                                               
BOOK9    CLC   1(2,RE),4(RE)       CHECK START IS NOT PAST END                  
         BNL   BADBOOK                                                          
         B     BOOKNEXT            ****  NO CHECKING NOW (JAN7/87)              
         SPACE 1                                                                
******** ***** ******************* ****  NO CHECKING NOW (JAN7/87)              
*        L     R1,ATWA                                                          
*        CLI   25(R1),2            IF WE ARE RUNNING SOON                       
*        BNE   BOOKNEXT                                                         
*        CLI   1(R1),C'*'          AND THIS IS NOT A DDS TERMINAL               
*        BE    BOOKNEXT                                                         
*        ZIC   RE,2(R4)            LIMIT TO 16 WEEKS                            
*        ZIC   RF,5(R4)                                                         
*        CLC   1(1,R4),4(R4)       CHECK FOR DIFFERENT YEARS                    
*        BH    BADBOOK                                                          
*        BE    *+8                                                              
*        LA    RF,48(RF)                                                        
*        LA    R4,3(R4)                                                         
*        SR    RF,RE                                                            
*        CH    RF,=H'16'                                                        
*        BL    BOOKNEXT                                                         
*        MVC   CONHEAD(L'SOONERR),SOONERR                                       
*        B     MYEND                                                            
         SPACE 1                                                                
BOOKNEXT LA    R3,32(R3)                                                        
         LA    R4,3(R4)                                                         
         AI    FIELDERR,1                                                       
         BCT   R0,BOOK2                                                         
         CLI   NUMBOOKS,0                                                       
         BE    BADBOOK                                                          
         CLI   MAX,0                                                            
         BE    XIT                                                              
         CLC   NUMBOOKS,MAX                                                     
         BH    BADBOOK                                                          
         MVC   ACTUAL,NUMBOOKS                                                  
         B     XIT                                                              
         SPACE 1                                                                
BADBOOK  MVC   CONHEAD(L'INVBOK),INVBOK                                         
         B     MYCURSOR                                                         
         EJECT                                                                  
BLKLIST  DC    AL1(00)             NORMAL HUTWEEK LIST                          
         DC    AL1(01,02,03,04)    JAN                                          
         DC    AL1(05,06,07,08)    FEB                                          
         DC    AL1(09,10,11,12)    MAR                                          
         DC    AL1(13,14,15,16)    APR (PLUS BLACK WEEK)                        
         DC    AL1(18,19,20,21)    MAY                                          
         DC    AL1(22,23,24,25)    JUN (PLUS BLACK WEEK)                        
         DC    AL1(27,28,29,30)    JUL                                          
         DC    AL1(31,32,33,34)    AUG (PLUS BLACK WEEK)                        
         DC    AL1(36,37,38,39)    SEP                                          
         DC    AL1(40,41,42,43)    OCT                                          
         DC    AL1(44,45,46,47)    NOV                                          
         DC    AL1(48,49,50,51)    DEC (PLUS BLACK WEEK)                        
         DC    AL1(17,26,35,52,53) BLACK WEEKS                                  
*                                                                               
BLK1984  DC    AL1(00)             1984 HUTWEEK LIST                            
         DC    AL1(01,02,03,04)    JAN                                          
         DC    AL1(05,06,07,08)    FEB                                          
         DC    AL1(09,10,11,12)    MAR                                          
         DC    AL1(13,14,15,16)    APR (PLUS BLACK WEEK)                        
         DC    AL1(18,19,20,21)    MAY                                          
         DC    AL1(22,23,24,25)    JUN (PLUS BLACK WEEK)                        
         DC    AL1(27,28,29,30)    JUL                                          
         DC    AL1(31,32,33,34)    AUG (PLUS 2 BLACK WEEKS)                     
         DC    AL1(37,38,39,40)    SEP                                          
         DC    AL1(41,42,43,44)    OCT                                          
         DC    AL1(45,46,47,48)    NOV                                          
         DC    AL1(49,50,51,52)    DEC (PLUS BLACK WEEK)                        
         DC    AL1(17,26,35,36,53) BLACK WEEKS                                  
*                                                                               
BLK1989  DC    AL1(00)             1984 HUTWEEK LIST                            
         DC    AL1(01,02,03,04)    JAN                                          
         DC    AL1(05,06,07,08)    FEB                                          
         DC    AL1(09,10,11,12)    MAR                                          
         DC    AL1(13,14,15,16)    APR (PLUS BLACK WEEK)                        
         DC    AL1(18,19,20,21)    MAY                                          
         DC    AL1(22,23,24,25)    JUN (PLUS BLACK WEEK)                        
         DC    AL1(27,28,29,30)    JUL                                          
         DC    AL1(31,32,33,34)    AUG (PLUS 2 BLACK WEEKS)                     
         DC    AL1(37,38,39,40)    SEP                                          
         DC    AL1(41,42,43,44)    OCT                                          
         DC    AL1(45,46,47,48)    NOV                                          
         DC    AL1(49,50,51,52)    DEC (PLUS BLACK WEEK)                        
         DC    AL1(17,26,35,52,36) BLACK WEEKS                                  
*                                                                               
         EJECT                                                                  
*              ROUTINE TO DEAL WITH ONE BOOK EXPRESSION                         
         SPACE 3                                                                
*              INPUT          (R2) P1=A(INPUT EXPRESSION)                       
*                             (R3) P2=LENGTH OF EXPRESSION                      
*                             (R4) P3=A(BOOK OUTPUT)                            
*              OUTPUT              NUMBOOKS INCREMENTED                         
*                                  CONDITION CODE SET                           
         SPACE 1                                                                
ONEBOOK  NTR1                                                                   
         LM    R2,R4,0(R1)                                                      
         SPACE 1                                                                
         GOTO1 DATVAL,PARAS,(R2),WORK                                           
         OC    PARAS(4),PARAS                                                   
         BNZ   ONE2                                                             
*                                                                               
         GOTO1 DATVAL,PARAS,(2,(R2)),WORK                                       
         OC    PARAS(4),PARAS                                                   
         BZ    ONE4                                                             
*                                                                               
ONE2     B     ONE4                                                             
         CLC   WORK(4),=CL4'8709'  TEST BOOK LESS THEN SEP/87                   
         BNL   *+8                                                              
         OI    BOOKOPT,X'80'                                                    
*                                                                               
         CLC   WORK(4),=CL4'8710'  TEST BOOK GREATER THEN OCT/87                
         BL    *+8                                                              
         OI    BOOKOPT,X'40'                                                    
*                                                                               
         CLC   WORK(4),=CL4'8609'  TEST BOOK LESS THEN SEP/86                   
         BNL   ONE4                                                             
         OI    BOOKOPT,X'20'                                                    
*                                  FIRST SEE IF BOOKVAL CAN HANDLE              
ONE4     XC    WORK,WORK                                                        
         STC   R3,WORK+5                                                        
         MVC   WORK+8(10),0(R2)                                                 
         MVC   DMCB+8(4),SCANNER                                                
         MVI   DMCB+8,C'N'         NET  VALIDATES FOR WEEK  BOOK                
         CLC   DBFILE,=C'NHI'      BUT NHI VALIDATES FOR MONTH                  
         BE    *+10                                                             
         CLC   DBFILE,=C'NAD'      AND NAD VALIDATES FOR MONTH                  
         BNE   *+8                                                              
         MVI   DMCB+8,C'S'                                                      
         GOTO1 BOOKVAL,DMCB,(C'N',WORK),(1,(R4))                                
         CLI   4(R1),1                                                          
         BE    ONEOK                                                            
         SPACE 1                                                                
*                                  NOW TRY FOR A DATE/NETWEEK APPROACH          
ONE8     CLC   DBFILE,=C'NAD'                                                   
         BE    ONEBAD                                                           
         CLC   DBFILE,=C'NHI'                                                   
         BE    ONEBAD                                                           
         GOTO1 DATVAL,PARAS,(R2),WORK                                           
         OC    PARAS(4),PARAS                                                   
         BZ    ONEBAD                                                           
         GOTO1 NETWEEK,PARAS,WORK,GETDAY,ADDAY                                  
         MVI   0(R4),X'50'                                                      
         MVC   1(1,R4),PARAS+4     YEAR NO.                                     
         MVC   2(1,R4),PARAS+8     WEEK NO.                                     
         B     ONEOK                                                            
         SPACE 1                                                                
ONEOK    AI    NUMBOOKS,1                                                       
*                                                                               
         CLC   DBFILE,=C'NHI'                                                   
         BE    ONEOK1                                                           
         CLC   DBFILE,=C'NAD'                                                   
         BE    ONEOK1                                                           
         CLC   1(2,R4),=XL2'5721'     TEST BOOK BEFORE SEP1/87                  
         BNL   *+8                                                              
         OI    BOOKOPT,X'80'                                                    
*                                                                               
         CLC   1(2,R4),=XL2'5725'     TEST BOOK AFTER OCT1/87                   
         BL    *+8                                                              
         OI    BOOKOPT,X'40'                                                    
*                                                                               
         CLC   1(2,R4),=XL2'5621'     TEST BOOK BEFORE SEP1/86                  
         BNL   *+8                                                              
         OI    BOOKOPT,X'20'                                                    
*                                                                               
ONEOK1   SR    R1,R1                                                            
         LTR   R1,R1                                                            
         B     XIT                                                              
         SPACE 1                                                                
ONEBAD   LA    R1,1                                                             
         LTR   R1,R1                                                            
         B     XIT                                                              
         SPACE 1                                                                
         EJECT                                                                  
*              ROUTINE TO CHECK FOR SOFT HUT EXPRESSIONS                        
         SPACE 1                                                                
*              ALLOWABLE           Q1/85      Q4/85                             
*                                  Q1/85,86   Q4/85,86                          
*                                  M1/85      M12/85                            
*                                  M1/85,86   M12/85,86                         
         SPACE 1                                                                
SOFTBOOK NTR1                                                                   
         CLC   DBFILE,=C'NAD'      NOT APPLICABLE FOR NAD                       
         BE    SOFTNO                                                           
         CLC   DBFILE,=C'NHI'      NOT APPLICABLE FOR NHI                       
         BE    SOFTNO                                                           
         CLI   MAX,1               MUST BE REQUESTING MULTIPLE BOOKS            
         BE    SOFTNO                                                           
         LA    R3,BLOCK+12                                                      
         LA    R3,3(R3)            SPACE TO YEAR                                
         CLI   0(R3),C'/'          GET PAST /                                   
         BNE   *+8                                                              
         LA    R3,1(R3)                                                         
         CLI   0(R3),C'0'          MUST BE NUMERIC                              
         BL    SOFTNO                                                           
         CLI   1(R3),C'0'                                                       
         BL    SOFTNO                                                           
         CLI   2(R3),C' '          AND EXPRESSION MUST END THERE                
         BH    SOFTNO                                                           
         PACK  DUB,0(2,R3)                                                      
         CVB   RE,DUB                                                           
         L     R1,=A(HUTTABLE)         CHECK FOR A MATCH ON PHRASE              
         CH    RE,=H'89'                                                        
         BNE   *+8                                                              
         L     R1,=A(HUTTAB89)                                                  
         A     R1,RELO                                                          
         LA    R3,BLOCK+12                                                      
         SPACE 1                                                                
SB2      ZIC   RE,0(R1)            LENGTH OF PHRASE                             
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R3),1(R1)                                                    
         BE    SB4                                                              
         LA    R1,7(R1)                                                         
         CLI   0(R1),X'FF'                                                      
         BE    SOFTNO                                                           
         B     SB2                                                              
         SPACE 1                                                                
SB4      CLC   DBFILE,=C'NTI'      FOR NTI                                      
         DROP  R6                                                               
         BNE   *+8                                                              
         OI    BOOKS,X'01'         SOFT EXPRESSION IS A RANGE                   
         MVC   BOOKS+2(1),5(R1)    START WEEK                                   
         MVC   BOOKS+5(1),6(R1)    END WEEK                                     
         LA    R3,1(RE,R3)         SPACE TO YEAR                                
         CLI   0(R3),C'/'          GET PAST /                                   
         BNE   *+8                                                              
         LA    R3,1(R3)                                                         
         CLI   0(R3),C'0'          MUST BE NUMERIC                              
         BL    SOFTNO                                                           
         CLI   1(R3),C'0'                                                       
         BL    SOFTNO                                                           
         CLI   2(R3),C' '          AND EXPRESSION MUST END THERE                
         BH    SOFTNO                                                           
         PACK  DUB,0(2,R3)                                                      
         AP    DUB,=P'100'         ALWAYS ASSUME A DATE AFTER 1999              
         CVB   R1,DUB                                                           
         STC   R1,BOOKS+1          START YEAR                                   
         STC   R1,BOOKS+4          END YEAR                                     
*                                                                               
         CLC   BOOK+1(2),=XL2'5721'   TEST BOOK BEFORE SEP1/87                  
         BNL   *+8                                                              
         OI    BOOKOPT,X'80'                                                    
*                                                                               
         CLC   BOOK+1(2),=XL2'5725'   TEST BOOK AFTER OCT1/87                   
         BL    *+8                                                              
         OI    BOOKOPT,X'40'                                                    
*                                                                               
         CLC   BOOK+1(2),=XL2'5621'   TEST BOOK BEFORE SEP1/86                  
         BNL   *+8                                                              
         OI    BOOKOPT,X'20'                                                    
         CLI   BLOCK+32,0          IS THERE A SECOND EXPRESSION                 
         BE    SOFTYES                                                          
         ZIC   R0,BLOCK+39         YES - IT MUST BE ANOTHER YEAR                
         LTR   R0,R0                                                            
         BZ    SOFTNO                                                           
         CR    R0,R1                                                            
         BL    SOFTNO                                                           
         STC   R0,BOOKS+4                                                       
         SPACE 1                                                                
SOFTYES  SR    R1,R1                                                            
         B     *+8                                                              
         SPACE 1                                                                
SOFTNO   LA    R1,1                                                             
         LTR   R1,R1                                                            
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO EXPLODE BOOK RANGE TO LIST                            
         SPACE 3                                                                
*              INPUT               MAX=MAX TO OUTPUT                            
*                                  BOOKS MAY CONTAIN BOOK-BOOK                  
*                                  ALTERNATE IF ALTOPT=Y                        
*              OUTPUT              BOOK,BOOK,BOOK                               
*                                  NUMBOOKS                                     
         SPACE 1                                                                
EXPBOOK  TM    BOOKS,X'01'         CHECK IF RANGE REQUESTED                     
         BNO   XIT                                                              
         NI    BOOKS,X'FE'                                                      
         MVI   NUMBOOKS,1          RESET NUMBOOKS                               
         ZIC   R3,BOOKS+1          PRESENT YEAR (R3)                            
         ZIC   R4,BOOKS+2          PRESENT WEEK (R4)                            
         ZIC   R0,MAX              REQUESTED MAX (R0)                           
         LA    R5,BOOKS+1                                                       
         LTR   R0,R0                                                            
         BNZ   *+8                                                              
         LA    R0,10               OR DEFAULT 10                                
         MVC   DUB(2),BOOKS+4      SAVE END OF RANGE Y/W                        
         SPACE 1                                                                
EXPBOOK2 STC   R3,0(R5)            COMPARE PRESENT Y/W                          
         STC   R4,1(R5)                                                         
         CLC   DUB(2),0(R5)                                                     
         BNH   XIT                                                              
         AI    NUMBOOKS,1                                                       
         LA    R4,1(R4)            BUMP WEEK BY 1                               
         CLI   ALTOPT,C'Y'         OR ALTERNATIVELY 2                           
         BNE   *+8                                                              
         LA    R4,1(R4)                                                         
         CH    R4,=H'49'                                                        
         BL    EXPBOOK4                                                         
         LA    R3,1(R3)                                                         
         SH    R4,=H'48'                                                        
         SPACE 1                                                                
EXPBOOK4 LA    R5,3(R5)                                                         
         BCT   R0,EXPBOOK2                                                      
         B     BADBOOK                                                          
         EJECT                                                                  
*              ROUTINE TO VALIDATE DEMO STRING                                  
         SPACE 3                                                                
VALDEM   GOTO1 ANY                                                              
         GOTO1 SCANNER,PARAS,(R2),(8,BLOCK),C',=,.'                             
*--IF SOURCE IS NTI OR P NAD DEMOS ARE INVALID EX. (67.RWMN1224)                
         L     RE,ARESSRCE                                                      
         CLC   8(3,RE),=CL3'NHI'                                                
         BE    VALD100                                                          
         CLC   8(3,RE),=CL3'NAD'                                                
         BE    VALD100                                                          
         LA    RE,BLOCK                                                         
         LA    RF,8                                                             
*                                                                               
VALD50   CLI   0(RE),0                                                          
         BE    VALD100                                                          
         CLI   1(RE),0             IS DEMO IN NAD FORMAT                        
         BNE   VALD60              YES? DOUBLE CHECK                            
*                                                                               
         L     R1,ARESSRCE         TCAR HAS TO USE NAD FORMAT DEMOS             
         CLC   8(3,R1),=C'WB1'                                                  
         BE    BADDEM                                                           
*                                                                               
VALD60   L     R1,ARESSRCE                                                      
         CLC   8(3,R1),=C'WB1'    TCAR HAVE TO USE NAD FORMAT DEMOS             
         BE    VALD70                                                           
*                                                                               
         TM    2(RE),X'80'         IS THERE NUMERIC PREFIX                      
         BNZ   BADDEM              YES ERROR                                    
*                                                                               
VALD70   LA    RE,32(RE)                                                        
         BCT   RF,VALD50                                                        
*--IF BOOK PRIOR TO SEP/88 THEN C'O'(TP PUT)                                    
*--OR A C'Q'(TP SHARE)                                                          
VALD100  TM    BOOKOPT,X'80'       IS THERE A BOOK PRIOR TO SEP/87              
         BZ    VALD200             NO                                           
*                                                                               
         LA    RE,BLOCK                                                         
         LA    RF,8                                                             
VALD160  CLI   0(RE),0                                                          
         BE    VALD200                                                          
         CLI   12(RE),C'O'                                                      
         BE    BADDEM                                                           
         CLI   12(RE),C'Q'                                                      
         BE    BADDEM                                                           
         CLI   22(RE),C'O'                                                      
         BE    BADDEM                                                           
         CLI   22(RE),C'Q'                                                      
         BE    BADDEM                                                           
         LA    RE,32(RE)                                                        
         BCT   RF,VALD160                                                       
*                                                                               
VALD200  LA    R5,DBLOCKB                                                       
         USING DBLOCKD,R5                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBSELMED,SVMEDIA                                                 
         MVC   DBFILE,SVFILE                                                    
         CLC   DBFILE,=C'NHI'                                                   
         BNE   *+10                                                             
         MVC   DBFILE,=C'NAD'                                                   
         MVC   DBDEMTYP,NEDEMTYP                                                
         GOTO1 DEMOVAL,PARAS,(NFLDS,(R2)),(MAX,DEMOS),(0,(R5))                  
         CLI   4(R1),0                                                          
         BE    BADDEM                                                           
         MVC   ACTUAL,4(R1)        PASS USER BACK NUMBER FOUND                  
         MVC   DEMMOD,DEMOS+1                                                   
         MVC   NUMDEMS,ACTUAL                                                   
         B     XIT                                                              
         SPACE 1                                                                
BADDEM   MVC   CONHEAD(L'INVDEM),INVDEM   INVALID DEMO                          
         B     MYEND                                                            
         DROP  R5                                                               
         EJECT                                                                  
*              VALIDATE NETWORK                                                 
         SPACE 3                                                                
VALNET   GOTO1 ANY                                                              
         AI    NUMNETS,1           UPDATE NUMBER OF NETWORKS                    
         L     R1,ARESSRCE         SKIP THIS FOR PROGRAM                        
         BAS   RE,NEWSTCHK         NEW STATION EDITS                            
         BNZ   BADNET                                                           
         CLI   8(R1),C'P'                                                       
         BE    VALNT8                                                           
         LA    R5,DBLOCKA                                                       
         USING DBLOCKD,R5                                                       
         MVC   ACTNET(4),8(R2)                                                  
         OC    ACTNET(4),=C'    '                                               
         MVC   ACTNET4,ACTNET+3    SAVE ACTUAL 4TH CHAR                         
*                                                                               
         CLC   8(5,R2),=C'ZZZZM'   NAD SYNDICATION                              
         BNE   *+14                                                             
         MVC   ACTNET+3(2),=C' M'                                               
         B     EXIT                                                             
*                                                                               
         CLC   8(5,R2),=C'ZZZZC'                                                
         BNE   *+14                                                             
         MVC   ACTNET+3(2),=C' C'                                               
         B     EXIT                                                             
*                                                                               
         CLI   12(R2),C'M'         DEAL WITH NAD SYNDICATION                    
         BNE   *+14                                                             
         MVC   ACTNET+3(2),=C' M'                                               
         B     EXIT                                                             
*                                                                               
         CLI   12(R2),C'H'         DEAL WITH NHI FILE (SIMILAR TO NAD)          
         BNE   VALNHX                                                           
         MVC   ACTNET+3(2),=C' H'                                               
         CLI   NADSECT,0                                                        
         BE    EXIT                                                             
         CLI   NADSECT,C'P'                                                     
         BE    EXIT                                                             
         MVC   ACTNET+3(1),NADSECT                                              
         B     EXIT                                                             
VALNHX   DS    0H                                                               
*                                                                               
         CLI   12(R2),C'S'         DEAL WITH SYNDICATION                        
         BNE   *+14                                                             
         MVC   ACTNET+3(2),=C' S'                                               
         B     EXIT                                                             
*                                                                               
         MVI   ACTNET+4,C'W'       FUDGE FOR SILLY INPUT SYNTAX                 
         OI    ACTNET+3,X'40'                                                   
         CLI   ACTNET+3,C'-'       REPLACE DASH WITH A SPACE                    
         BNE   *+8                 FOR 3-CHARACTER STATIONS                     
         MVI   ACTNET+3,C' '                                                    
*                                                                               
         L     RF,ACOMFACS                                                      
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTO1 (RF),DMCB,NECABCLL  GET A(NET CABLE STATIONS TABLE)              
         ICM   RE,15,0(R1)         A(TABLE) RETURNED IN P1                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     RF,4(R1)            L'TABLE ENTRY RETURNED IN P2                 
*                                                                               
         USING NECBCLD,RE                                                       
         SPACE 1                                                                
VALCB    CLC   NECBALPH,ACTNET                                                  
         BE    EXIT                                                             
         AR    RE,RF               POINT TO NEXT ENTRY IN THE TABLE             
         CLI   0(RE),X'FF'                                                      
         BNE   VALCB                                                            
         DROP  RE                                                               
*                                                                               
         MVC   ACTNET+3(2),=C' T'                                               
*                                                                               
         L     R1,=A(VALNTLST)     CHECK BASIC LIST                             
         A     R1,RELO                                                          
         SPACE 1                                                                
VALNT2   CLC   0(3,R1),ACTNET                                                   
         BE    VALNT4                                                           
         LA    R1,3(R1)                                                         
         CLI   0(R1),X'FF'                                                      
         BNE   VALNT2                                                           
         B     VALNT8                                                           
         SPACE 1                                                                
VALNT4   CLC   DBFILE,=C'NAD'      NAD HAS SPECIAL REQUIREMENTS                 
         BNE   VALNT6                                                           
         MVC   ACTNET+3(1),NADSECT        4'TH CHARACTER SECTION                
         MVI   ACTNET+4,C'N'          AND 5'TH IS N                             
*                                  (RETURN ABCT-N)                              
         CLI   NADSECT,C'P'        THERE IS NO SECTION P                        
         BNE   XIT                                                              
         CLC   ACTNET(3),=C'HUT'   FOR HUT                                      
         BNE   XIT                                                              
         B     BADNET                                                           
         SPACE 1                                                                
VALNT6   CLI   11(R2),C'S'         TEST FOR SEASON-TO-DATE                      
         BNE   *+12                                                             
         MVI   ACTNET+3,C'S'       (RETURN ABCS-T)                              
         B     XIT                                                              
         SPACE 1                                                                
         CLI   11(R2),C'A'         TEST FOR ASCRIBED                            
         BNE   *+12                                                             
         MVI   ACTNET+4,C'A'       (RETURN ABC -A)                              
         B     XIT                                                              
         SPACE 1                                                                
         CLI   11(R2),X'00'        TEST FOR ASCRIBED (NUL)                      
         BNE   *+12                                                             
         MVI   ACTNET+4,C'A'       (RETURN ABC -A)                              
         B     XIT                                                              
         SPACE 1                                                                
         CLI   11(R2),C' '         TEST FOR ASCRIBED (BLANK)                    
         BNE   *+12                                                             
         MVI   ACTNET+4,C'A'       (RETURN ABC -A)                              
         B     XIT                                                              
         SPACE 1                                                                
         CLI   11(R2),C'I'         TEST FOR INTEGRATED                          
         BNE   *+12                                                             
         MVI   ACTNET+4,C'I'       (RETURN ABC -I)                              
         B     XIT                                                              
         SPACE 1                                                                
         CLI   11(R2),C'D'         TEST FOR DIARY                               
         BNE   *+12                                                             
         MVI   ACTNET+4,C'D'       (RETURN ABC -D)                              
         B     XIT                                                              
         SPACE 1                                                                
         CLI   11(R2),C'C'         TEST FOR CONFORMED                           
         BNE   *+12                                                             
         MVI   ACTNET+4,C'C'       (RETURN ABC -C)                              
         B     XIT                                                              
         SPACE 1                                                                
         CLI   11(R2),C'Z'         TEST FOR Z-BOOK (TEST)                       
         BNE   BADNET                                                           
         MVI   ACTNET+4,C'Z'       (RETURN ABC -Z)                              
         B     XIT                                                              
         SPACE 2                                                                
*              VALIDATE NETWORK - READING FROM 'STATION' FILE                   
         SPACE 1                                                                
*                                                                               
VALNT8   MVC   ACTNET(4),8(R2)                                                  
         OI    ACTNET+3,X'40'                                                   
         MVI   ACTNET+4,C'N'                                                    
         MVC   KEY(17),=C'SNABC NAA00000000'                                    
         USING STAREC,R4                                                        
         LA    R4,KEY                                                           
         MVC   STAKCALL,ACTNET                                                  
         MVC   STAKAGY,AGENCY                                                   
         MVI   USEIO,C'Y'                                                       
         MVC   FILENAME,=C'STATION'                                             
         GOTO1 READ                                                             
         PACK  DUB,SMKT            CONVERT 'MARKET' NUMBER                      
         CVB   R1,DUB                                                           
         STH   R1,DUB                                                           
         MVC   ACTMKT,DUB                                                       
         MVI   USEIO,C'N'                                                       
         XC    FILENAME,FILENAME                                                
         B     XIT                                                              
         SPACE 1                                                                
BADNET   MVC   CONHEAD(L'INVNET),INVNET    INVALID NETWORK                      
         B     MYEND                                                            
         DROP  R5                                                               
*--ADDITIONAL EDITS PUT INTO THE RESEARCH SYSTEM                                
NEWSTCHK NTR1                                                                   
         L     RE,ARESSRCE                                                      
         CLI   8(RE),C'P'                                                       
         BNE   NSTC50                                                           
*                                                                               
         CLC   8(5,R2),=CL5'ZZZZM'                                              
         BE    NSTCERR                                                          
         CLC   8(5,R2),=CL5'ZZZZS'                                              
         BE    NSTCERR                                                          
         CLC   8(5,R2),=CL5'ZZZZC'                                              
         BE    NSTCERR                                                          
******************                                                              
NSTC50   CLC   8(3,RE),=CL3'NAD'                                                
         BNE   NSTC100                                                          
*                                                                               
         CLI   11(R2),C'S'         TEST SEASON TO DATE                          
         BE    NSTCERR                                                          
*                                                                               
         CLC   8(5,RE),=CL5'NAD,D'                                              
         BNE   NSTC52                                                           
         CLI   12(R2),C'H'                                                      
         BE    NSTC52                                                           
         CLC   8(3,R2),=CL3'HUT'                                                
         BNE   NSTCERR                                                          
*                                                                               
NSTC52   CLC   8(5,R2),=CL5'ZZZZM'                                              
         BE    NSTCERR                                                          
         CLC   8(5,R2),=CL5'ZZZZS'                                              
         BE    NSTCERR                                                          
         CLC   8(5,R2),=CL5'ZZZZC'                                              
         BE    NSTCERR                                                          
******************                                                              
NSTC100  CLC   8(3,RE),=CL3'NTI'                                                
         BNE   NSTC150                                                          
*                                                                               
         CLC   8(4,R2),=C'WTBS'    BECAUSE OF STUPID WAY CALL LTRS              
         BE    NSTCEXIT             ARE DONE                                    
*        CLI   11(R2),C'S'         TEST SEASON TO DATE                          
*        BE    NSTCERR                                                          
*                                                                               
         CLC   8(5,R2),=CL5'ZZZZM'                                              
         BNE   *+10                                                             
         CLC   8(5,R2),=CL5'ZZZZC'                                              
         BNE   *+10                                                             
         CLC   8(5,R2),=CL5'ZZZZS'                                              
         BNE   NSTC150                                                          
         TM    BOOKOPT,X'80'       CHECK BOOK BEFORE SEP1/87                    
         BNZ   NSTCERR                                                          
******************                                                              
NSTC150  TM    BOOKOPT,X'40'       CHECK BOOK AFTER OCT1/87                     
         BZ    NSTC170                                                          
         CLI   11(R2),C'D'         TEST FOR DIARY                               
         BE    NSTCERR                                                          
*                                                                               
NSTC170  TM    BOOKOPT,X'20'       CHECK BOOK BEFORE SEP1/86                    
         BZ    NSTCEXIT                                                         
         CLI   11(R2),C'D'         TEST FOR DIARY                               
         BNE   NSTCERR                                                          
******************                                                              
NSTCEXIT SR    RE,RE                                                            
NSTCERR  LTR   RE,RE                                                            
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE DAY                                                     
         SPACE 1                                                                
VALDAY   GOTO1 ANY                                                              
         LA    R3,DAYTBL                                                        
         SR    R4,R4                                                            
         SPACE 1                                                                
VALDAY2  STC   R4,ACTUAL                                                        
         CLC   0(3,R3),8(R2)                                                    
         BE    XIT                                                              
         LA    R3,3(R3)                                                         
         LA    R4,1(R4)                                                         
         CLI   0(R3),X'FF'                                                      
         BNE   VALDAY2                                                          
         SPACE 1                                                                
BADDAY   MVC   CONHEAD(L'INVDAYM),INVDAYM     INVALID DAY                       
         B     MYEND                                                            
         SPACE 1                                                                
DAYTBL   DC    C'M-FMONTUEWEDTHUFRISATSUNM-SVAR'                                
         DC    X'FF'                                                            
         EJECT                                                                  
*              VALIDATE DAY/TIME EXPRESSIONS                                    
         SPACE 3                                                                
*              INPUT               P1 BYTE 1 N'DAY/TIMES                        
         SPACE 1                                                                
VALDYTM  L     R3,0(R1)                                                         
         ST    R2,SAVER2                                                        
         MVI   DAYTIMES,0                                                       
         XC    DAYTMLST,DAYTMLST                                                
         LA    R5,DAYTMLST                                                      
         SPACE 1                                                                
         GOTO1 ANY                 1 DAY REQUIRED                               
         SPACE 1                                                                
DT1      CLI   5(R2),0             DAY                                          
         BNE   DT2                 - OK                                         
         BAS   RE,BUMP                                                          
         CLI   5(R2),0             TIME                                         
         BNE   DT5                 - ERROR                                      
         B     DT12                - LOOK AT NEXT PAIR OF FIELDS                
*                                                                               
DT2      MVI   0(R5),X'FF'         CHECK FOR 'ALL'                              
         CLC   8(3,R2),=C'ALL'                                                  
         BE    DT6                                                              
*                                                                               
         GOTO1 SCANNER,PARAS,(R2),(1,BLOCK),C',=,-'                             
         CLI   BLOCK,0                                                          
         BE    DT5                                                              
         CLI   BLOCK+1,0                                                        
         BNE   DT4                                                              
         L     R4,=A(DAYLIST)                                                   
         A     R4,RELO                                                          
         SR    RF,RF                                                            
         SPACE 1                                                                
DT3      STC   RF,0(R5)                                                         
         SR    RE,RE                                                            
         ICM   RE,1,BLOCK                                                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   BLOCK+12(0),0(R4)                                                
         BE    DT6                                                              
         LA    R4,9(R4)                                                         
         LA    RF,1(RF)                                                         
         CLI   0(R4),X'FF'                                                      
         BNE   DT3                                                              
         B     DT5                                                              
         SPACE 1                                                                
*--CHECK DAY RANGES (MONDAY-FRIDAY OR MONDAY-SATURDAY)                          
DT4      SR    RE,RE                                                            
         ICM   RE,1,BLOCK                                                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   BLOCK+12(0),=CL6'MONDAY'                                         
         BNE   DT5                                                              
*                                                                               
         MVI   0(R5),0                                                          
         SR    RE,RE                                                            
         ICM   RE,1,BLOCK+1                                                     
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   BLOCK+22(0),=CL6'FRIDAY'                                         
         BE    DT6                                                              
*                                                                               
         MVI   0(R5),8                                                          
         SR    RE,RE                                                            
         ICM   RE,1,BLOCK+1                                                     
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   BLOCK+22(0),=CL6'SUNDAY'                                         
         BE    DT6                                                              
*                                                                               
DT5      MVC   CONHEAD(L'INVDAYM),INVDAYM  INVALID DAY                          
         B     MYEND                                                            
         SPACE 1                                                                
DT6      BAS   RE,BUMP             LOOK AT TIME FIELD                           
         IC    R4,5(R2)                                                         
         MVI   1(R5),X'FF'         CHECK FOR 'ALL'                              
         CLC   8(3,R2),=C'ALL'                                                  
         BE    DT10                                                             
         LTR   R4,R4                                                            
         BZ    DT10                                                             
         GOTO1 TIMVAL,PARAS,((R4),8(R2)),1(R5)                                  
         CLI   0(R1),X'FF'                                                      
         BNE   DT8                                                              
DT7      MVC   CONHEAD(L'INVTIM),INVTIM                                         
         B     MYEND                                                            
         EJECT                                                                  
DT8      OC    3(2,R5),3(R5)       TEST FOR END TIME                            
         BNZ   *+10                                                             
         MVC   3(2,R5),1(R5)       NO-SET END TIME=START TIME                   
         SPACE 1                                                                
         MVC   DUB,1(R5)           ADJUST TIME FOR 12-6AM                       
         LH    R1,DUB                                                           
         CH    R1,=H'600'                                                       
         BNL   *+8                                                              
         AH    R1,=H'2400'                                                      
         STH   R1,DUB                                                           
         LH    R1,DUB+2                                                         
         CH    R1,=H'600'                                                       
         BH    *+8                                                              
         AH    R1,=H'2400'                                                      
         STH   R1,DUB+2                                                         
         CLC   DUB+2(2),DUB        ENSURE END GT OR EQ START                    
         BL    DT7                                                              
         MVC   1(4,R5),DUB                                                      
         SPACE 1                                                                
DT10     ZIC   R1,DAYTIMES         INCREMENT DAY/TIME LIST COUNT                
         LA    R1,1(R1)                                                         
         STC   R1,DAYTIMES                                                      
         LA    R5,5(R5)                                                         
         SPACE 1                                                                
DT12     BAS   RE,BUMP             NEXT DAY/DETAIL FIELD                        
         BCT   R3,DT1                                                           
*                                                                               
         BAS   RE,CKOVLAP          CHECK DAY TIME OVERLAP                       
         BZ    XIT                 IF OK EXIT                                   
         L     R2,SAVER2                                                        
         MVC   CONHEAD(L'INVTIM),INVDTOV    ERROR                               
         B     MYEND                                                            
         EJECT                                                                  
*--ROUTINE MAKE SURE NO DAY TIME OVERLAPS                                       
CKOVLAP  NTR1                                                                   
         LA    R3,DAYTMLST                                                      
CKOV50   LA    R4,DAYTMLST                                                      
         L     R2,SAVER2                                                        
CKOV60   CR    R3,R4               ARE WE AT SAME LOCATION                      
         BE    CKOV80              YES BYPASS                                   
         CLI   0(R3),X'FF'         IS DAY=ALL                                   
         BE    CKOV70                                                           
         CLI   0(R4),X'FF'         IS DAY=ALL                                   
         BE    CKOV70                                                           
         CLC   0(1,R3),0(R4)       ARE DAYS THE SAME                            
         BNE   CKOV80              NO BYPASS                                    
CKOV70   CLI   1(R3),X'FF'         IS TIME=ALL                                  
         BE    CKOVERR                                                          
         CLI   1(R4),X'FF'         IS TIME=ALL                                  
         BE    CKOVERR                                                          
         CLC   1(2,R3),3(R4)       IS START1 GREATER THEN END2                  
         BNL   CKOV80              YES BYPASS                                   
         CLC   3(2,R3),1(R4)       IS END1 LESS THEN START2                     
         BNH   CKOV80              YES BYPASS                                   
         B     CKOVERR             ELSE ERROR CONDITION                         
CKOV80   BAS   RE,BUMP                                                          
         BAS   RE,BUMP                                                          
         LA    R4,5(R4)                                                         
         OC    0(5,R4),0(R4)       IS TABLE TWO AT END                          
         BNZ   CKOV60                                                           
         LA    R3,5(R3)                                                         
         OC    0(5,R3),0(R3)       IS TABLE ONE AT END                          
         BNZ   CKOV50                                                           
*                                                                               
CKOVEXT  SR    R3,R3                                                            
CKOVERR  BAS   RE,BUMP                                                          
         ST    R2,SAVER2                                                        
         LTR   R3,R3                                                            
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE TIME EXPRESSION                                         
         SPACE 3                                                                
VALTIM   GOTO1 ANY                                                              
         CLC   8(3,R2),=CL3'VAR'                                                
         BE    BADTIME                                                          
*                                                                               
         ZIC   R3,5(R2)                                                         
         GOTO1 TIMVAL,PARAS,((R3),8(R2)),WORK                                   
         CLI   0(R1),X'FF'                                                      
         BE    BADTIME                                                          
         SPACE 1                                                                
VALTIM2  LH    R1,WORK             CONVERT MILITARY TO QUARTER HOUR             
         CH    R1,=H'600'                                                       
         BNL   *+8                                                              
         LA    R1,2400(R1)                                                      
         SH    R1,=H'600'                                                       
         SR    R0,R0                                                            
         D     R0,=F'100'                                                       
         SLL   R1,2                HOURS IN R1                                  
         LR    R3,R1               1/4S IN R3                                   
         SRDA  R0,32                                                            
         D     R0,=F'15'                                                        
         AR    R3,R1                                                            
         STC   R3,ACTUAL                                                        
         B     XIT                                                              
BADTIME  MVC   CONHEAD(L'INVTIM),INVTIM                                         
         B     MYEND                                                            
         EJECT                                                                  
*              VALIDATE DEMO PERFORMANCE LEVEL                                  
         SPACE 3                                                                
*                                  IF DEMMOD IS R,S,X 1 DEC                     
*                                  IF DEMMOD IS P,T,Q INTEGER                   
*              OUTPUT              VALUE RETURNED IN WORK                       
         SPACE 1                                                                
VALDLVL  XC    WORK,WORK                                                        
         SPACE 1                                                                
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         XR    R1,R1                                                            
         IC    R1,5(R2)            LENGTH                                       
         LA    R3,8(R2)            DATA                                         
         SPACE 1                                                                
         MVI   ERROR,2                                                          
         CH    R1,=H'5'            NO MORE THAN 5 DIGITS                        
         BH    ERREND                                                           
         SPACE 1                                                                
DLVL10   CLI   0(R3),C'.'                                                       
         BNE   DLVL40                                                           
         CLI   DEMMOD,C'R'                                                      
         BE    DLVL20                                                           
         CLI   DEMMOD,C'S'                                                      
         BE    DLVL20                                                           
         CLI   DEMMOD,C'X'                                                      
         BE    DLVL20                                                           
         L     RE,=A(DLVL1)                                                     
         A     RE,RELO                                                          
         MVC   CONHEAD(L'DLVL1),0(RE)                                           
         B     MYEND                                                            
         SPACE 1                                                                
DLVL20   CH    R1,=H'2'            DECIMAL IS NEXT TO LAST                      
         BE    DLVL30                                                           
         L     RE,=A(DLVL2)                                                     
         A     RE,RELO                                                          
         MVC   CONHEAD(L'DLVL2),0(RE)                                           
         B     MYEND                                                            
DLVL30   MVI   ERROR,3             MUST BE NUMERIC                              
         CLI   1(R3),C'0'                                                       
         BL    ERREND                                                           
         CLI   1(R3),C'9'                                                       
         BH    ERREND                                                           
         SPACE 1                                                                
         PACK  DUB,1(1,R3)                                                      
         CVB   RF,DUB                                                           
         CLI   5(R2),2                                                          
         BE    DLVL80              DECIMAL IN FIRST POSITION                    
         IC    R1,5(R2)                                                         
         SH    R1,=H'3'                                                         
         B     DLVL50              DO WHOLE NUMBER                              
         SPACE 1                                                                
DLVL40   MVI   ERROR,3             MUST BE NUMERIC                              
         CLI   0(R3),C'0'                                                       
         BL    ERREND                                                           
         CLI   0(R3),C'9'                                                       
         BH    ERREND                                                           
         LA    R3,1(R3)                                                         
         BCT   R1,DLVL10                                                        
         SPACE 1                                                                
         IC    R1,5(R2)                                                         
         BCTR  R1,0                                                             
         SPACE 1                                                                
DLVL50   EX    R1,*+8                                                           
         B     DLVL60                                                           
         PACK  DUB,8(0,R2)                                                      
DLVL60   CVB   RE,DUB                                                           
         CLI   DEMMOD,C'S'                                                      
         BE    DLVL70                                                           
         CLI   DEMMOD,C'R'                                                      
         BE    DLVL70                                                           
         CLI   DEMMOD,C'X'                                                      
         BNE   DLVL80                                                           
DLVL70   MH    RE,=H'10'                                                        
         SPACE 1                                                                
DLVL80   AR    RE,RF                                                            
         ST    RE,FULL                                                          
         MVC   WORK(4),FULL                                                     
         SPACE 1                                                                
         B     XIT                                                              
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
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING AGYKEY,R4                                                        
         MVI   AGYKTYPE,X'06'                                                   
         MVC   AGYKAGY,AGENCY                                                   
         GOTO1 READ                                                             
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
GETAGYX  B     XIT                                                              
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
*              ALLOW MY OWN ERROR MESSAGES TO BE USED                           
         SPACE 3                                                                
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
TITOUT   MVC   WORK,RESTITLE       USE STANDARD                                 
         CLI   OWNTITLE,0                                                       
         BE    *+10                                                             
         MVC   WORK,OWNTITLE       OR OWN TITLE                                 
         GOTO1 CENTER,DMCB,WORK,40 (CENTERED)                                   
         LA    R2,H1+30                                                         
         CLI   HOWWIDE,132         MAY BE WIDE PRINT                            
         BNE   *+8                                                              
         LA    R2,H1+40                                                         
         MVC   0(40,R2),WORK       MOVE IN TITLE                                
*                                  AND UNDERLINE IT                             
         GOTO1 UNDERLIN,DMCB,(40,0(R2)),(X'BF',132(R2))                         
         B     XIT                                                              
         EJECT                                                                  
*              STANDARD HEADLINE ROUTINES                                       
         SPACE 3                                                                
RESHEAD  GOTO1 VTITOUT             DEAL WITH TITLES (ABOVE)                     
         L     R2,ARESSRCE         PICK UP A(SOURCE)                            
         LA    R3,H4                                                            
         LTR   R2,R2                                                            
         BZ    RH2                                                              
         CLI   5(R2),0                                                          
         BE    RH2                                                              
         MVC   0(6,R3),=C'SOURCE'                                               
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,SCRTOHED                                                      
         BAS   RE,RESPSE           PROGRAM START/END                            
         LA    R3,132(R3)                                                       
         SPACE 1                                                                
RH2      L     R2,ARESBOOK         NOW DEAL WITH BOOK                           
         LTR   R2,R2                                                            
         BZ    RH4                                                              
         CLI   5(R2),0                                                          
         BE    RH4                                                              
         MVC   0(4,R3),=C'BOOK'                                                 
         CLI   NUMBOOKS,1                                                       
         BE    *+8                                                              
         MVI   4(R3),C'S'                                                       
         L     R1,ARESSRCE                                                      
         CLI   8(R1),C'P'          SHOW HUT FOR PROGRAM                         
         BNE   RH3                                                              
         MVC   0(5,R3),=C'HUT  '                                                
         SPACE 1                                                                
RH3      ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,SCRTOHED                                                      
         SPACE 1                                                                
RH4      CLI   THISNET,0           OPTIONAL NETWORK                             
         BE    RH6                                                              
         LA    R3,H5+76                                                         
         CLI   HOWWIDE,132                                                      
         BNE   *+8                                                              
         LA    R3,H5+98                                                         
         MVC   0(10,R3),=C'NETWORK - '                                          
         MVC   10(4,R3),THISNET                                                 
         CLI   13(R3),C'A'         TEST ASCRIBED                                
         BNE   *+8                                                              
         MVI   13(R3),C' '         REPLACE 'A' WITH BLANK                       
         EJECT                                                                  
*              DEAL WITH HEADINGS AND BOXES                                     
         SPACE 3                                                                
RH6      OC    RESTITA,RESTITA                                                  
         BZ    *+10                                                             
         MVC   H8,RESTITA                                                       
         OC    RESTITB,RESTITB                                                  
         BZ    *+10                                                             
         MVC   H9,RESTITB                                                       
         OC    RESTITC,RESTITC                                                  
         BZ    *+10                                                             
         MVC   H10,RESTITC                                                      
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
         MVC   BOXCOLS,RESCOLS                                                  
         MVI   BOXROWS+6,C'T'                                                   
         MVI   BOXROWS+58,C'B'                                                  
         LA    R3,BOXROWS+10                                                    
         CLC   H10,SPACES                                                       
         BNE   RH8                                                              
         BCTR  R3,0                                                             
         CLC   H9,SPACES                                                        
         BNE   RH8                                                              
         BCTR  R3,0                                                             
         SPACE 1                                                                
RH8      MVI   0(R3),C'M'                                                       
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO SHOW PROGRAM START END                                
         SPACE 3                                                                
RESPSE   NTR1                                                                   
         CLI   8(R2),C'P'                                                       
         BNE   XIT                                                              
         BAS   RE,BUMP                                                          
         BAS   RE,BUMP                                                          
         LA    R3,2(R3)                                                         
         CLI   5(R2),0                                                          
         BE    RESPSE2                                                          
         ZIC   R1,5(R2)                                                         
         EX    R1,SCRTOHED                                                      
         SPACE 1                                                                
RESPSE2  BAS   RE,BUMP                                                          
         BAS   RE,BUMP                                                          
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         MVI   18(R3),C'-'                                                      
         LA    R3,9(R3)                                                         
         ZIC   R1,5(R2)                                                         
         EX    R1,SCRTOHED                                                      
         B     XIT                                                              
         SPACE 1                                                                
SCRTOHED MVC   10(0,R3),8(R2)                                                   
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
         CH    R1,=H'6'                                                         
         BL    *+8                                                              
         LA    R1,6                                                             
         LTR   R1,R1                                                            
         BZ    VALFILT4                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),12(R3)      SAVE THIS FILTER                             
         LA    R4,6(R4)            AND ADDRESS THE NEXT AREA                    
         SPACE 1                                                                
VALFILT4 LA    R3,32(R3)                                                        
         AI    FIELDERR,1                                                       
         BCT   R0,VALFILT2                                                      
*  CHECK ALL FILTERS EITHER NEGATIVE OR POSITIVE                                
         LA    R0,10                                                            
         LA    R4,FILTERS                                                       
VALFILT6 OC    0(6,R4),0(R4)                                                    
         BZ    XIT                                                              
         CLI   FILTERS,C'-'                                                     
         BE    VALFILT7                                                         
         CLI   0(R4),C'-'                                                       
         BE    BADFILT                                                          
         B     VALFILT8                                                         
VALFILT7 CLI   0(R4),C'-'                                                       
         BNE   BADFILT                                                          
VALFILT8 LA    R4,6(R4)                                                         
         BCT   R0,VALFILT6                                                      
         B     XIT                                                              
         SPACE 1                                                                
BADFILT  L     RE,=A(INVFLT)                                                    
         A     RE,RELO                                                          
         MVC   CONHEAD(L'INVFLT),0(RE)                                          
         B     MYCURSOR                                                         
         EJECT                                                                  
*              ROUTINE TO CHECK FILTERS                                         
         SPACE 3                                                                
*              INPUT               P1=A(FIELD TO BE FILTERED)                   
*                                  FILTERS HAS UP TO 10                         
*                                  6 BYTE FILTER EXPRESSIONS                    
*              OUTPUT              RETURNS CONDITION CODE                       
         SPACE 1                                                                
CHEFILT  L     R4,0(R1)            CHECK FILTERS                                
         LA    R2,FILTERS          CHECK FILTERS                                
         OC    0(6,R2),0(R2)       ARE ANY SPECIFIED?                           
         BZ    CHEFYES             NO - SO ITS OK                               
         LA    R0,10               UP TO TEN MAY BE SPECIFIED                   
         CLI   FILTERS,C'-'        NEGATIVE FILTERS                             
         BE    CHEF3                                                            
         SPACE 1                                                                
CHEF2    OC    0(6,R2),0(R2)       IF ANY IS SPECIFIED                          
         BZ    CHEFNO                                                           
         BAS   RE,CHEF4            GO CHECK                                     
         BE    CHEFYES             FOR ANY ONE TO BE SATISFIED                  
         LA    R2,6(R2)                                                         
         BCT   R0,CHEF2                                                         
         B     CHEFNO              NONE PASSED SO NO GOOD                       
*                                                                               
CHEF3    OC    0(6,R2),0(R2)       IF ANY IS SPECIFIED                          
         BZ    CHEFYES                                                          
         BAS   RE,CHEF4            GO CHECK                                     
         BNE   CHEFNO              FOR ANY ONE TO BE SATISFIED                  
         LA    R2,6(R2)                                                         
         BCT   R0,CHEF3                                                         
         B     CHEFYES             NONE PASSED SO NO GOOD                       
         SPACE 1                                                                
CHEF4    NTR1                                                                   
*                                  R2=A(THIS FILTER EXPRESSION)                 
         LR    R3,R4                                                            
         LA    R0,4                                                             
         CLI   0(R2),C'-'          NEGATIVE FILTER                              
         BE    CHEF20                                                           
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
         B     CHEFYES                                                          
         SPACE 1                                                                
CHEF20   LA    R2,1(R2)                                                         
CHEF25   CLI   0(R2),C'*'          * IS WILD                                    
         BE    CHEF30                                                           
         CLI   0(R2),0             SO IS ZERO                                   
         BE    CHEF30                                                           
         CLI   0(R2),C'?'          QUESTION SIGN IS SPECIAL CHAR MATCH          
         BE    CHEF28                                                           
         CLC   0(1,R2),0(R3)       MUST MATCH                                   
         BNE   CHEFYES                                                          
         B     CHEF30                                                           
         SPACE 1                                                                
CHEF28   CLI   0(R3),C' '          MATCH ON ANY SPECIAL CHARACTER               
         BH    CHEFYES             INCLUDING SPACE AND BINARY ZERO              
         SPACE 1                                                                
CHEF30   LA    R2,1(R2)                                                         
         LA    R3,1(R3)                                                         
         BCT   R0,CHEF25                                                        
CHEFNO   LA    R1,1                                                             
         B     *+6                                                              
         SPACE 1                                                                
CHEFYES  SR    R1,R1                                                            
         LTR   R1,R1                                                            
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PICK UP HUTS FOR PROGRAM                              
         SPACE 3                                                                
*              INPUTS              HUTTIME REQUESTED START END                  
*                                  DAYNUM REQUESTED DAY NUMBER                  
*              OUTPUT              HUT RETURNED IN HUT                          
         SPACE 1                                                                
PROGHUT  CLI   HUTSW,0             INITIALIZE FIRST TIME THROUGH                
         BNE   *+8                                                              
         BAS   RE,HUTINIT                                                       
         MVI   HUTSW,1                                                          
         ZIC   R3,DAYNUM                                                        
         MH    R3,=H'96'                                                        
         LA    R3,HUTVALS(R3)      PICK UP HUTS FOR DAY                         
         MVC   HUTLIST(96),0(R3)                                                
         LA    R4,KEY                                                           
         BAS   RE,GETAHUTS                                                      
         MVC   0(96,R3),HUTLIST    ROUTINE UPDATES VALUES                       
         B     XIT                                                              
         SPACE 1                                                                
HUTINIT  NTR1                      LOAD UP HUT VALUES                           
         LA    R3,HUTVALS                                                       
         LA    R4,9                                                             
         SPACE 1                                                                
HUTINIT2 XC    0(96,R3),0(R3)                                                   
         LA    R3,96(R3)                                                        
         BCT   R4,HUTINIT2                                                      
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINES TO COMPUTE HUTS                                         
         SPACE 3                                                                
GETAHUTS NTR1                                                                   
         LA    R2,HUTTIME          CONVERT TIME TO QUARTERS                     
         LA    R3,HUTQ                                                          
         BAS   RE,GETQ                                                          
         MVC   HUTQ+1(1),HUTQ                                                   
         LA    R2,HUTTIME+2                                                     
         LA    R3,HUTQ+1                                                        
         OC    0(2,R2),0(R2)                                                    
         BZ    *+8                                                              
         BAS   RE,GETQ                                                          
         SPACE 1                                                                
         SR    R2,R2               ADD HUTS IN R2                               
         LA    R3,1                COUNT IN R3                                  
         SPACE 1                                                                
GETA2    ZIC   R1,HUTQ                                                          
         SRL   R1,1                                                             
         SLL   R1,1                                                             
         LA    R1,HUTLIST(R1)      LOOK UP HUT FOR THIS 1/2 HOUR                
         BAS   RE,GETDHUTS                                                      
         AH    R2,0(R1)                                                         
         AI    HUTQ,2                                                           
         CLC   HUTQ(1),HUTQ+1                                                   
         BNL   GETA4                                                            
         LA    R3,1(R3)                                                         
         B     GETA2                                                            
         SPACE 1                                                                
GETA4    LR    R0,R2               AVERAGE HUTS                                 
         SRDA  R0,31                                                            
         DR    R0,R3                                                            
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         STH   R1,HUT                                                           
         B     XIT                                                              
         EJECT                                                                  
*              REFRESH HUT VALUE FOR SPECIFIC DAYNUM HUTQ                       
         SPACE 3                                                                
GETDHUTS NTR1                                                                   
         OC    0(2,R1),0(R1)       HAVE WE LOOKED THIS UP BEFORE                
         BNZ   XIT                                                              
         LR    R2,R1                                                            
         LA    R5,HUTBLOCK         SET UP BLOCK FOR GETHUT                      
         USING GETHUTD,R5                                                       
         XC    GHBLOCK,GHBLOCK                                                  
         MVI   GHPREVYR,C'N'       DON'T TRY PREV. YR. HUTS                     
         MVC   GHREPDAY,DAYNUM                                                  
         MVC   GHQUARTS,HUTQ                                                    
         MVC   GHQUARTS+1,HUTQ                                                  
         MVI   GHSCHEME,X'FE'      PRESET FROM YEAR RECORDS                     
         MVI   GH52,C'Y'           PRESET 52 WEEK OPTION                        
         CLI   HUT52,0                                                          
         BE    *+10                                                             
         MVC   GH52,HUT52                                                       
         MVC   GHBKTYPE,HUTTYPE    HUT TYPE (D, A, I OR C)                      
*                                  DEFAULT IS ASCRIBED                          
         CLI   GHBKTYPE,0                                                       
         BNE   *+8                                                              
         MVI   GHBKTYPE,C'A'                                                    
*                                                                               
         CLI   GHBKTYPE,C'D'                                                    
         BNE   *+8                                                              
         MVI   GHBKTYPE,C'O'       (GETHUT MUST'NT CHANGE 'D' TO X'00')         
         MVC   GHBOOKS(2),BOOKS+1                                               
         MVC   GHBOOKS+2(2),BOOKS+4                                             
         MVC   GHCOMFCS,ACOMFACS                                                
         MVC   GHAGY,AGENCY        FOR LOCKOUTS                                 
         CLI   HUTSCHEM,0                                                       
         BE    GETD2                                                            
         MVC   GHSCHEME,HUTSCHEM   AGENCY SCHEME REQUESTED                      
         MVC   GHAGYMED,BINAGYMD                                                
         SPACE 1                                                                
GETD2    GOTO1 GETHUT,DMCB,(R5)                                                 
         MVC   0(2,R2),GHHUT                                                    
         CLI   HUTSCHEM,0                                                       
         BE    XIT                                                              
         GOTO1 HIGH                NEED TO RESTORE SEQUENCE                     
         B     XIT                                                              
         SPACE 1                                                                
GETQ     NTR1                                                                   
         LH    R1,0(R2)            MILITARY TIME TO 1/4 HOUR                    
         SR    R0,R0                                                            
         D     R0,=F'100'                                                       
         CH    R1,=H'6'                                                         
         BNL   *+8                                                              
         LA    R1,24(R1)                                                        
         SH    R1,=H'6'                                                         
         SLL   R1,2                                                             
         LR    R2,R1                                                            
         SRDA  R0,32                                                            
         D     R0,=F'15'                                                        
         AR    R2,R1                                                            
         STC   R2,0(R3)                                                         
         B     XIT                                                              
         EJECT                                                                  
*        ADJUSTMENTS FOR ASCRIBED, INTEGRATED, DIARY & CONFORMED                
         SPACE 2                                                                
ADJSEL   LA    R5,DBLOCKA                                                       
         USING DBLOCKD,R5                                                       
         CLC   DBFILE,=C'NAD'      NAD OR NTI                                   
         BNE   ADJSEL0                                                          
         MVI   DBBTYPE,X'00'       SET TO NAD                                   
         CLI   ACTNET+4,C'M'                                                    
         BE    XIT                                                              
         CLI   ACTNET+4,C'H'                                                    
         BE    XIT                                                              
         CLI   ACTNET4,C' '                                                     
         BNH   XIT                                                              
         MVC   DBBTYPE,ACTNET4                                                  
         B     XIT                                                              
*                                                                               
ADJSEL0  CLI   DBSELSTA+4,C'H'    NHI TIME PERIODS                              
         BNE   *+16                                                             
         MVI   DBSELSTA+3,C' '                                                  
         MVI   DBBTYPE,0                                                        
         B     XIT                                                              
*                                                                               
         CLI   DBSELSTA+4,C'W'    REALLY CABLE                                  
         BNE   *+16                                                             
         MVI   DBSELSTA+4,C'C'                                                  
         MVI   DBBTYPE,X'00'                                                    
         B     XIT                                                              
*                                                                               
         MVI   DBBTYPE,C'A'        PRESET TO ASCRIBED                           
         SPACE 1                                                                
         CLI   DBSELSTA+4,C'A'     CHECK FOR ASCRIBED                           
         BNE   ADJSEL1                                                          
         MVI   DBSELSTA+4,C'T'     AND MODIFY DBLOCK                            
         MVI   DBBTYPE,C'A'                                                     
         B     XIT                                                              
         SPACE 1                                                                
ADJSEL1  CLI   DBSELSTA+4,C'I'     CHECK FOR INTEGRATED                         
         BNE   ADJSEL2                                                          
         MVI   DBSELSTA+4,C'T'     AND MODIFY DBLOCK                            
         MVI   DBBTYPE,C'I'                                                     
         B     XIT                                                              
         SPACE 1                                                                
ADJSEL2  CLI   DBSELSTA+4,C'D'     CHECK FOR DIARY                              
         BNE   ADJSEL3                                                          
         MVI   DBSELSTA+4,C'T'     AND MODIFY DBLOCK                            
         MVI   DBBTYPE,C'D'                                                     
         B     XIT                                                              
         SPACE 1                                                                
ADJSEL3  CLI   DBSELSTA+4,C'C'     CHECK FOR CONFORMED                          
         BNE   ADJSEL4                                                          
         MVI   DBSELSTA+4,C'T'     AND MODIFY DBLOCK                            
         MVI   DBBTYPE,C'C'                                                     
         B     XIT                                                              
         SPACE 1                                                                
ADJSEL4  CLI   DBSELSTA+4,C'Z'     CHECK FOR Z-BOOK (TEST)                      
         BNE   ADJSEL9                                                          
         MVI   DBSELSTA+4,C'T'     AND MODIFY DBLOCK                            
         MVI   DBBTYPE,C'Z'                                                     
         B     XIT                                                              
         SPACE 1                                                                
ADJSEL9  B     XIT                                                              
         SPACE 3                                                                
DISPNET  LA    R5,DBLOCKA          RETURN EDITED NETWORK IN WORK                
         MVC   WORK(4),DBSELSTA                                                 
         CLC   =C'PAR ',DBSELSTA                                                
         BNE   *+10                                                             
         MVC   WORK(4),=C'UPN '                                                 
         CLI   DBSELSTA+4,C'M'     NAD SYND USES 4 CHAR                         
         BE    XIT                                                              
         CLI   DBSELSTA+4,C'C'     CABLE USES 4 CHAR                            
         BE    XIT                                                              
         MVI   WORK+3,C' '         THREE CHARACTERS ONLY FOR NAD                
         CLC   DBFILE,=C'NAD'                                                   
         BE    XIT                                                              
         MVC   WORK+3(1),DBBTYPE   USE BOOK TYPE AS 4TH CHARACTER               
         B     XIT                                                              
         DROP  R5                                                               
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
         DC    X'1415E026303316170932'                                          
CORES    EQU   (*-CORETAB)                                                      
         EJECT                                                                  
*              LTORG FOR CONTROLLER                                             
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ERROR MESSAGES FOR CONTROLLER                                    
         SPACE 3                                                                
INVSRC   DC    C'** ERROR ** INVALID SOURCE'                                    
INVBOK   DC    C'** ERROR ** INVALID BOOK'                                      
INVDEM   DC    C'** ERROR ** INVALID DEMO'                                      
INVNET   DC    C'** ERROR ** INVALID NETWORK'                                   
INVDAYM  DC    C'** ERROR ** INVALID DAY EXPRESSION'                            
INVTIM   DC    C'** ERROR ** INVALID TIME EXPRESSION'                           
INVDTOV  DC    C'** ERROR ** DAYS AND TIMES OVERLAP'                            
INVFIL   DC    C'** ERROR ** INVALID FILE'                                      
INVFLT   DC    C'** ERROR ** INVALID FILTER EXPRESSION'                         
INVDAT   DC    C'** ERROR ** INVALID DATE'                                      
ENDERR   DC    C'** ERROR ** END BEFORE START'                                  
SOONERR  DC    C'** ERROR ** SOON LIMITED TO 16 WEEKS'                          
DLVL1    DC    C'** ERROR ** INVALID DEMO PERFORMANCE LEVEL'                    
DLVL2    DC    C'** ERROR ** FORMAT IS XX.X'                                    
         EJECT                                                                  
         SPACE 1                                                                
VALNTLST DC    C'ABCCBSNBCFOXHUT'  NETWORKS                                     
         DC    C'WB PARUPNPAXTEL'  NETWORKS                                     
         DC    C'TF UNICW MNTAZA'  NETWORKS                                     
         DC    C'IONUMA'           NETWORKS                                     
         DC    C'INDSUPPBSPAYCAB'  NON-NETWORKS                                 
         DC    C'CATXINXSUFAFAGA'  NON-NETWORKS                                 
         DC    C'AGBAGCAGDAGEAGF'  NON-NETWORKS                                 
         DC    X'FF'                                                            
*&&DO                                                                           
VALCBLST DC    C'AED AEN FAM FNN TNT DSC '  CABLE 1                             
         DC    C'USA VH1 HLN WTBSTNN CNN '  CABLE 2                             
         DC    C'SHOWMTV ESPNNICKLIF HBO '  CABLE 3                             
         DC    C'CMAXTMC CNBCTBS DISCBET '  CABLE 4                             
         DC    C'CMDYTOONENT SCIFTLC TWC '  CABLE 5                             
         DC    X'FF'                                                            
*&&                                                                             
         SPACE 1                                                                
DAYLIST  DC    C'YYYYYYYYY'        DUMMY ENTRY                                  
         DC    C'MONDAY   '                                                     
         DC    C'TUESDAY  '                                                     
         DC    C'WEDNESDAY'                                                     
         DC    C'THURSDAY '                                                     
         DC    C'FRIDAY   '                                                     
         DC    C'SATURDAY '                                                     
         DC    C'SUNDAY   '                                                     
         DC    C'YYYYYYYYY'        DUMMY ENTRY                                  
         DC    C'VARIABLE '                                                     
         DC    X'FF'                                                            
         SPACE 1                                                                
HUTTABLE DC    AL1(3),C'Q1/ ',AL1(1,12)      QUARTERS                           
         DC    AL1(3),C'Q2/ ',AL1(13,50)                                        
         DC    AL1(3),C'Q3/ ',AL1(25,36)                                        
         DC    AL1(3),C'Q4/ ',AL1(37,52)                                        
*                                                                               
         DC    AL1(3),C'M1/ ',AL1(1,4)       MONTHS                             
         DC    AL1(3),C'M2/ ',AL1(5,8)                                          
         DC    AL1(3),C'M3/ ',AL1(9,12)                                         
         DC    AL1(3),C'M4/ ',AL1(13,49)                                        
         DC    AL1(3),C'M5/ ',AL1(17,20)                                        
         DC    AL1(3),C'M6/ ',AL1(21,50)                                        
         DC    AL1(3),C'M7/ ',AL1(25,28)                                        
         DC    AL1(3),C'M8/ ',AL1(29,51)                                        
         DC    AL1(3),C'M9/ ',AL1(33,36)                                        
         DC    AL1(4),C'M10/',AL1(37,40)                                        
         DC    AL1(4),C'M11/',AL1(41,44)                                        
         DC    AL1(4),C'M12/',AL1(45,52)                                        
*                                                                               
         DC    AL1(3),C'JAN ',AL1(1,4)       ALPHA MONTH EXPRESSIONS            
         DC    AL1(3),C'FEB ',AL1(5,8)                                          
         DC    AL1(3),C'MAR ',AL1(9,12)                                         
         DC    AL1(3),C'APR ',AL1(13,49)                                        
         DC    AL1(3),C'MAY ',AL1(17,20)                                        
         DC    AL1(3),C'JUN ',AL1(21,50)                                        
         DC    AL1(3),C'JUL ',AL1(25,28)                                        
         DC    AL1(3),C'AUG ',AL1(29,51)                                        
         DC    AL1(3),C'SEP ',AL1(33,36)                                        
         DC    AL1(3),C'OCT ',AL1(37,40)                                        
         DC    AL1(3),C'NOV ',AL1(41,44)                                        
         DC    AL1(3),C'DEC ',AL1(45,52)                                        
*                                                                               
         DC    X'FF'                                                            
*                                                                               
         SPACE 1                                                                
HUTTAB89 DC    AL1(3),C'Q1/ ',AL1(1,12)      QUARTERS                           
         DC    AL1(3),C'Q2/ ',AL1(13,50)                                        
         DC    AL1(3),C'Q3/ ',AL1(25,51)                                        
         DC    AL1(3),C'Q4/ ',AL1(35,52)                                        
*                                                                               
         DC    AL1(3),C'M1/ ',AL1(1,4)       MONTHS                             
         DC    AL1(3),C'M2/ ',AL1(5,8)                                          
         DC    AL1(3),C'M3/ ',AL1(9,12)                                         
         DC    AL1(3),C'M4/ ',AL1(13,49)                                        
         DC    AL1(3),C'M5/ ',AL1(17,20)                                        
         DC    AL1(3),C'M6/ ',AL1(21,50)                                        
         DC    AL1(3),C'M7/ ',AL1(25,28)                                        
         DC    AL1(3),C'M8/ ',AL1(29,51)                                        
         DC    AL1(3),C'M9/ ',AL1(33,36)                                        
         DC    AL1(4),C'M10/',AL1(37,40)                                        
         DC    AL1(4),C'M11/',AL1(41,44)                                        
         DC    AL1(4),C'M12/',AL1(45,52)                                        
*                                                                               
         DC    AL1(3),C'JAN ',AL1(1,4)       ALPHA MONTH EXPRESSIONS            
         DC    AL1(3),C'FEB ',AL1(5,8)                                          
         DC    AL1(3),C'MAR ',AL1(9,12)                                         
         DC    AL1(3),C'APR ',AL1(13,49)                                        
         DC    AL1(3),C'MAY ',AL1(17,20)                                        
         DC    AL1(3),C'JUN ',AL1(21,50)                                        
         DC    AL1(3),C'JUL ',AL1(25,28)                                        
         DC    AL1(3),C'AUG ',AL1(29,51)                                        
         DC    AL1(3),C'SEP ',AL1(33,36)                                        
         DC    AL1(3),C'OCT ',AL1(37,40)                                        
         DC    AL1(3),C'NOV ',AL1(41,44)                                        
         DC    AL1(3),C'DEC ',AL1(45,52)                                        
*                                                                               
         DC    X'FF'                                                            
*                                                                               
         EJECT                                                                  
*              TABLES OF RECORDS ACTIONS AND COMBINATIONS                       
         SPACE 3                                                                
RECACTS  DS    0D                                                               
         SPACE 1                                                                
* X'01' ENTRIES ARE AVAILABLE RECORDS                                           
*                                                                               
* CL8 EXPANDED RECORD NAME                                                      
* CL1 RECORD NUMBER (01 IS RESERVED)                                            
* CL1 PHASE NUMBER FOR DATA DICTIONARY - LEAVE AS ZERO FOR NOW                  
* CL1 PHASE NUMBER FOR HELP SCREEN - LEAVE AS ZERO FOR NOW                      
         SPACE 1                                                                
         DC    X'01',C'MESSAGE ',AL1(08),X'0000'                                
         SPACE 2                                                                
* X'04' ENTRIES ARE AVAILABLE REPORTS                                           
*                                                                               
* CL8 EXPANDED REPORT NAME                                                      
* CL1 REPORT NUMBER (01 IS RESERVED)                                            
* CL1 PHASE NUMBER FOR DATA DICTIONARY - LEAVE AS ZERO FOR NOW                  
* CL1 PHASE NUMBER FOR HELP SCREEN - LEAVE AS ZERO FOR NOW                      
         SPACE 1                                                                
         DC    X'04',C'PURE    ',AL1(01),X'0000'                                
         DC    X'04',C'FLEXI   ',AL1(02),X'0000'                                
         DC    X'04',C'LAYOUT  ',AL1(03),X'0000'                                
         DC    X'04',C'TREND   ',AL1(04),X'0000'                                
*        DC    X'04',C'RESEARCH',AL1(05),X'0000'                                
         DC    X'04',C'NPT     ',AL1(06),X'0000'                                
         DC    X'04',C'CORRECT ',AL1(07),X'0000'                                
         DC    X'04',C'GRID    ',AL1(09),X'0000'                                
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
         DC    X'02',C'REPORT  ',AL1(12,12,00)                                  
         DC    X'02',C'HELP    ',AL1(00,00,00)                                  
         EJECT                                                                  
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
         DC    X'03',AL1(01,12),X'F101000148',C'PUWR'  PURE     REPORT          
         DC    X'03',AL1(02,12),X'F202000248',C'FLWR'  FLEXI    REPORT          
         DC    X'03',AL1(03,12),X'F303000348',C'LAWR'  LAYOUT   REPORT          
         DC    X'03',AL1(04,12),X'F404000448',C'TRWR'  TREND    REPORT          
         DC    X'03',AL1(05,12),X'F505001578',C'REWR'  RESEARCH REPORT          
         DC    X'03',AL1(06,12),X'F606000648',C'PTWR'  PROGRAM  TREND           
         DC    X'03',AL1(07,12),X'F707000748',C'COWR'  CORRECT  REPORT          
         DC    X'03',AL1(08,10),X'F808000081',C'    '  MESSAGE  LIST            
         DC    X'03',AL1(09,12),X'F920002048',C'GRWR'  GRID     REPORT          
         DC    X'FF'                                                            
         SPACE 3                                                                
*              PHASE USED UP BY SYSTEM SO FAR                                   
         SPACE 1                                                                
*              X0 X1 X2 X3 X4 X5 X6 X7 X8 X9 XA XB XC XD XE XF                  
*                                                                               
*        0X     Y  Y  Y  Y  Y  Y  Y  Y  Y                                       
*        1X                    Y                                                
*        FX        Y  Y  Y  Y  Y  Y  Y  Y                    Y                  
         EJECT                                                                  
       ++INCLUDE NERESALL1                                                      
         EJECT                                                                  
*              CTGENFILE                                                        
*              SPGENAGY                                                         
*              SPGENSTA                                                         
*              DDCOMFACS                                                        
*              FAFACTS                                                          
*              FATIOB                                                           
*              NEGETHUTD                                                        
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE SPGENAGY                                                       
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FATIOB                                                         
       ++INCLUDE NEGETHUTD                                                      
       ++INCLUDE DEDEMTABD                                                      
         PRINT ON                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'150NERES00   06/09/17'                                      
         END                                                                    
