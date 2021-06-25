*          DATA SET SPRES00    AT LEVEL 085 AS OF 03/02/09                      
*PHASE T20F00A,*                                                                
*INCLUDE MEDGET                                                                 
*INCLUDE SPGETIUN                                                               
*INCLUDE TWABLD                                                                 
*INCLUDE SQUASHER                                                               
*INCLUDE NUMVAL                                                                 
*INCLUDE KHDUMMY                                                                
*--------------------------------------------------------------------           
* MAKE SURE THAT THE 'INCLUDE KHDUMMY' IS THE LAST INCLUDE                      
*--------------------------------------------------------------------           
         TITLE 'T20F00 - SPOT RESEARCH CONTROLLER'                              
T20F00   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LENWORK,**T20F00,RA,RR=R2,CLEAR=YES                              
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
         ST    R9,ASYSD                                                         
         SPACE 1                                                                
         ST    R1,SYSPARMS         THIS CODE FOR CURSOR POSITIONING             
         L     R0,0(R1)                                                         
         ST    R0,ATIOB                                                         
*                                                                               
         L     R3,4(R1)                                                         
         ST    R3,ATWA             R3=A(TWA)                                    
         USING CONHEADH-64,R3                                                   
*                                                                               
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(131),SPACES                                             
*                                                                               
         ST    RB,SYSRB            SYSTEM BASE REGS                             
         ST    RA,SYSRA                                                         
         BAS   RE,SYSINIT          INITIALIZE SYSTEM DEPENDENT                  
*                                                                               
         BAS   RE,SETRD            SET RD SO GENCON WILL RETURN                 
*                                                                               
         XC    DMCB,DMCB           OBTAIN CORE-RESIDENT ADDRESSES               
         LA    R2,CORETAB                                                       
         LA    R0,CORES            COUNTER                                      
         LA    R4,COREFACS         POINT TO ADDRESS AREA                        
         L     RF,CALLOV                                                        
         LA    R1,DMCB                                                          
         MVC   DMCB+4(3),=X'D9000A'                                             
         SPACE 1                                                                
RES10    CLI   0(R2),0             TEST NOP PHASE                               
         BE    RES12                                                            
         MVC   DMCB+7(1),0(R2)                                                  
         GOTO1 (RF),(R1),0                                                      
         MVC   0(4,R4),DMCB        SAVE MODULE ADDRESS                          
*                                                                               
RES12    LA    R2,1(R2)            NEXT MODULE NUMBER                           
         LA    R4,4(R4)            NEXT ADDRESS                                 
         BCT   R0,RES10                                                         
*                                                                               
         LA    R0,GOMSPACK                                                      
         ST    R0,MSPACK                                                        
         LA    R0,GOMSUNPK                                                      
         ST    R0,MSUNPK                                                        
         SPACE 1                                                                
         LA    R0,BLOCK            SET DEFAULT SCANNER TABLE ADDRESS            
         ST    R0,SCANADDR                                                      
         MVI   SCANLEN,32          AND DEFAULT ENTRY LENGTH                     
         SPACE 1                                                                
         GOTO1 GENCON,DMCB,(R8)     CALL GENCON                                 
         SPACE 1                                                                
EXIT     XIT1                                                                   
         EJECT                                                                  
SETRD    NTR1                                                                   
         ST    RD,SYSRD            SET RD SO GENCON WILL RETURN CONTROL         
         B     EXIT                                                             
         SPACE 1                                                                
* INITIALIZE FOR SYSTEM                                                         
         SPACE 1                                                                
SYSINIT  NTR1                                                                   
         SPACE 1                   GET TERMINAL VALUES                          
         MVI   DDS,C'N'                                                         
         CLI   TWAOFFC,C'*'        TEST FOR DDS TERMINAL                        
         BNE   *+8                                                              
         MVI   DDS,C'Y'                                                         
         OI    GENSTAT1,APPLIC     MAKE THIS CONTROLLER CORE-RES                
*                                  TO GIVE ME MORE ROOM IN PRGM-AREA            
         MVC   TERM,TWATRM                                                      
         MVC   AUTH,TWAAUTH                                                     
         MVC   USERID,TWAORIG                                                   
         MVC   AGENCY,TWAAGY                                                    
*                                                                               
         MVI   TWANSAVE,0          NEED THIS FOR GENCON                         
         SPACE 1                                                                
SYS1     LA    R5,SYSV                                                          
         LA    R6,SYSVCON                                                       
         LA    R7,NVTYPES                                                       
         SPACE 1                                                                
SYS2     L     R1,0(R6)            RELOCATE SYSTEM VTYPES                       
         A     R1,RELO                                                          
         ST    R1,0(R5)                                                         
         LA    R5,4(R5)                                                         
         LA    R6,4(R6)                                                         
         BCT   R7,SYS2                                                          
         SPACE 1                                                                
         LA    R5,VCOMMON          SET UP COMMON ENTRIES                        
         SR    R6,R6                                                            
         LA    R7,SYSCOMM                                                       
         LA    R0,VCOUNT                                                        
         SPACE 1                                                                
SYS4     ST    R5,0(R7)                                                         
         STC   R6,0(R7)                                                         
         LA    R6,4(R6)                                                         
         LA    R7,4(R7)                                                         
         BCT   R0,SYS4                                                          
         SPACE 1                                                                
*                                  SEED DSECT FOR DUMP READABILITY              
         MVC   DUMPSYSD,=C'**SYSD**'                                            
         MVC   DUMPCOMM,=C'*COMMON*'                                            
         MVC   DUMPBUFF,=C'**BUFF**'                                            
         MVC   DUMPHEAD,=C'**HEAD**'                                            
         MVC   DUMPDBLK,=C'*DBLOCK*'                                            
         EJECT                                                                  
* SET SYSTEM DEPENDENT VALUES                                                   
         SPACE 1                                                                
         MVI   SYSTEM,C'S'         SPOT                                         
         MVI   GETMSYS,5           GENCON MESSAGES                              
         SPACE 1                                                                
         MVI   MAXIOS,NIOS         USES 2 I/O AREAS                             
         MVC   SIZEIO,=AL4(LIOS)   EACH I/O IS 2000 BYTES                       
         MVC   SYSDUMMY,VDUMMY                                                  
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
         MVC   RCPROG(2),=C'SP'         PREFIX FOR REPORT NO.                   
         MVC   SYSPHASE,=X'D9020F00'    PRESET FOR SYSTEM CALLOVS               
         L     R1,=A(RECACTS)           RECORD/ACTION DIRECTORY                 
         A     R1,RELO                                                          
         ST    R1,ARECACT                                                       
         SPACE 1                                                                
* SET UP CERTAIN ADDRESSES                                                      
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
         MVC   DEMEL,CDEMEL                                                     
         L     RE,=V(SPGETIUN)                                                  
         A     RE,RELO                                                          
         ST    RE,VGETIUN                                                       
         L     RE,=V(TWABLD)                                                    
         A     RE,RELO                                                          
         ST    RE,VTWABLD                                                       
         L     RE,=V(SQUASHER)                                                  
         A     RE,RELO                                                          
         ST    RE,VSQUASH                                                       
         L     RE,=V(NUMVAL)                                                    
         A     RE,RELO                                                          
         ST    RE,VNUMVAL                                                       
         L     RE,=A(E1BTAB)                                                    
         A     RE,RELO                                                          
         ST    RE,E1BOOK                                                        
         B     XIT                                                              
         DROP  R4                                                               
         SPACE 2                                                                
IOLEN    EQU   NIOS*(LIOS+8)                                                    
         EJECT                                                                  
* SYSTEM ROUTINES ENTERABLE FROM BASE OF OVERLAY *                              
         DS    0H                                                               
VCOMMON  NTR1  BASE=SYSRB                                                       
         L     R3,ATWA                                                          
         L     R8,ASPOOLD                                                       
         L     R9,ASYSD            RESTORE POINTER TO SYSTEM STORAGE            
         L     RA,SYSRA            2ND BASE REG - R9 MUST BE SET FIRST          
         ST    RD,COMMRD                                                        
         SRL   RF,24                                                            
         B     VBRANCH(RF)                                                      
         SPACE 1                                                                
VBRANCH  B     GETAGY                                                           
         B     VALSRC                                                           
         B     VALBOOK                                                          
         B     VALDEM                                                           
         B     VALDAY                                                           
         B     VALTIM                                                           
         B     VALDYTM                                                          
         B     VALSTA                                                           
         B     GETERR                                                           
         B     CURSERR                                                          
         B     GETMKT                                                           
         B     CITYMRKT            CITY CODE --> MARKET NUMBER                  
         B     VALDLVL                                                          
         B     VALCATS                                                          
         B     RADBOOK                                                          
         B     VALDSTA                                                          
         B     GOGETIUN            SPGETIUN                                     
         B     MINIPAR                                                          
         B     CPRSERR                                                          
         B     CPRSERR             NOTHING                                      
         B     CPRSERR             NOTHING                                      
         B     CPRSERR             NOTHING                                      
         B     NUM2CODE            MARKET NUMBER TO MARKET CODE                 
         SPACE 1                                                                
VCOUNT   EQU   (*-VBRANCH)/4                                                    
         EJECT                                                                  
* GET AGENCY NAME/ADDRESS FROM CONTROL FILE ID RECORD *                         
         SPACE 1                                                                
GETAGY   XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING AGYHDR,R4                                                        
         MVI   KEY,6                                                            
         MVC   KEY+1(2),AGENCY                                                  
         MVI   USEIO,C'Y'                                                       
         MVC   AIO,AIO1                                                         
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         MVC   CTRY,AGYPROF+7                                                   
         DROP  R4                                                               
         MVC   FILENAME,=CL8'CTFILE'                                            
         MVI   USEIO,C'Y'                                                       
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTIKEY,R4                                                        
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),T20FFFD+10                                           
         MVC   AIO,AIO1                                                         
         GOTO1 READ                                                             
         L     R4,AIO                                                           
         LA    R6,CTIDATA                                                       
         MVI   ELCODE,X'36'                                                     
         BAS   RE,FIRSTEL                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CTORGD,R6                                                        
         MVC   USERNAME,CTORGNAM                                                
         MVC   USERADDR,CTORGADD                                                
         MVC   SVUSRNAM(66),USERNAME  SAVE FOR FUTURE REF                       
         MVC   SVCTRY,CTRY                                                      
         XC    FILENAME,FILENAME                                                
         MVI   USEIO,C'N'                                                       
         B     XIT                                                              
         EJECT                                                                  
*  VALIDATE SOURCE *                                                            
         SPACE 1                                                                
VALSRC   DS    0H                                                               
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBSELAGY,AGENCY     FOR MARKET SECURITY                          
*                                                                               
         GOTO1 ANY                                                              
         ZIC   RE,5(R2)            GET INPUT LENGTH                             
         BCTR  RE,0                SET FOR EX                                   
*                                                                               
         MVI   ERROR,INVSRC                                                     
         LA    R4,SRCLIST                                                       
         USING SRCLISTD,R4                                                      
*                                                                               
SRC10    EX    RE,CLCSRC                                                        
         BE    SRC20                                                            
         LA    R4,SRCNEXT                                                       
         CLI   0(R4),X'FF'                                                      
         BNE   SRC10                                                            
         B     GETERR                                                           
*                                                                               
SRC20    MVC   DBSELSRC,SRCDBSRC                                                
         MVC   DBFILE,SRCDBFIL                                                  
         MVC   8(6,R2),SRCINPUT                                                 
         OI    6(R2),X'80'         TRANSMIT FULL SOURCE NAME                    
         MVC   DBSELMED,SRCSELMD                                                
         MVC   BKVALSRC,SRCBKSRC                                                
*                                                                               
         MVC   BYTE,DBSELMED                                                    
         CLI   BYTE,C'C'                                                        
         BNE   *+8                                                              
         MVI   BYTE,C'T'                                                        
         GOTO1 =V(MEDGET),DMCB,(BYTE,AGENCY),DATAMGR,WORK,RR=RELO               
         MVC   BAGYMD,WORK         SAVE AGENCY/MEDIA BYTE                       
         CLI   8(R1),X'FF'                                                      
         BE    GETERR                                                           
         B     XIT                                                              
*                                                                               
CLCSRC   CLC   8(0,R2),SRCINPUT                                                 
*                                                                               
         DROP  R4                                                               
         EJECT                                                                  
SRCLIST  DS    0CL12                                                            
         DC    C'ARB   ',C'T',C'TP ',C'AA'                                      
         DC    C'BBM   ',C'C',C'TP ',C'AA'                                      
         DC    C'BBR   ',C'R',C'RDP',C'MM'                                      
         DC    C'CSI   ',C'C',C'TP ',C'NN'                                      
         DC    C'NSI   ',C'T',C'TP ',C'NN'                                      
         DC    C'SRC   ',C'T',C'TP ',C'SS'                                      
         DC    C'RARB  ',C'R',C'TP ',C'AA'                                      
         DC    C'RBIR  ',C'R',C'TP ',C'NN' **SHOULD BE SOURCE B                 
         DC    C'BIRCH ',C'R',C'TP ',C'NN' **                                   
         DC    C'DRARB ',C'R',C'RDP',C'AA'                                      
         DC    C'DRBIR ',C'R',C'RDP',C'NN' **                                   
         DC    C'RADAR ',C'R',C'TP ',C'RR'  NEW RADAR                           
         DC    X'FF'                                                            
         SPACE 2                                                                
SRCLISTD DSECT                                                                  
SRCINPUT DS    CL6                 VALID INPUT                                  
SRCSELMD DS    CL1                 DBSELMED VALUE                               
SRCDBFIL DS    CL3                 DBSELFIL VALUE                               
SRCDBSRC DS    CL1                 DBSELSRC VALUE                               
SRCBKSRC DS    CL1                 BOOKVAL SOURCE VALUE                         
SRCNEXT  EQU   *                                                                
         SPACE 1                                                                
T20F00   CSECT                                                                  
         EJECT                                                                  
*  VALIDATE BOOK(S) AND OUTPUT SERIES OF 4 BYTE FIELDS *                        
         SPACE 1                                                                
VALBOOK  DS    0H                                                               
         GOTO1 ANY                                                              
         SPACE 1                                                                
VALBOOK2 GOTO1 BOOKVAL,PARAS,(BKVALSRC,(R2)),(10,WORK),(C'B',SCANNER), X        
               BKTYPES                                                          
         MVI   ERROR,INVBOOK                                                    
         CLI   4(R1),0                                                          
         BE    GETERR                                                           
*                                                                               
         MVC   NBOOKS,4(R1)        PASS USER BACK NUMBER FOUND                  
*                                                                               
         ZIC   R0,NBOOKS                                                        
         LA    R5,BOOKS                                                         
         LA    R6,BKTYPES                                                       
         LA    R7,WORK                                                          
*                                                                               
BOOK10   MVC   0(3,R5),0(R7)       MOVE IN BOOK                                 
         MVC   3(1,R5),0(R6)       SAVE BKTYPE IN BOOK+3                        
         LA    R5,4(R5)                                                         
         LA    R6,1(R6)                                                         
         LA    R7,3(R7)                                                         
         BCT   R0,BOOK10                                                        
         B     XIT                                                              
         EJECT                                                                  
* VALIDATE RADIO BOOKS - OUTPUT 4 BYTE BOOKS IN BOOKS                           
*                               AND NUMBER OF BOOKS IN NBOOKS                   
         SPACE 1                                                                
RADBOOK  MVI   SCANLEN,32                                                       
         GOTO1 SCANNER,PARAS,(R2),(MAX,BLOCK),C',=,/'                           
*                                                                               
         LA    R4,BLOCK                                                         
         LA    R5,BOOKS                                                         
         ZIC   R0,PARAS+4                                                       
         MVI   ERROR,INVBOOK                                                    
         LTR   R0,R0                                                            
         BZ    GETERR                                                           
         STC   R0,NBOOKS                                                        
         SPACE 1                                                                
RBOOK2   ZIC   R1,0(R4)                                                         
         LTR   R1,R1                                                            
         BZ    GETERR                                                           
         BCTR  R1,0                                                             
         LA    RE,RBLIST           ARB AND BIRCH                                
         CLI   DBSELSRC,C'M'       BBM RADIO                                    
         BNE   *+8                                                              
         LA    RE,RBLISTM                                                       
         CLI   DBSELSRC,C'R'       RADAR RADIO                                  
         BNE   *+8                                                              
         LA    RE,RBLISTR                                                       
         SPACE 1                                                                
RBOOK4   EX    R1,*+8              CHECK LIST FOR SEASON MATCH                  
         B     *+10                                                             
         CLC   0(0,RE),12(R4)                                                   
         BE    RBOOK6                                                           
         LA    RE,8(RE)                                                         
         CLI   0(RE),X'FF'                                                      
         BNE   RBOOK4                                                           
         B     GETERR                                                           
         SPACE 1                                                                
RBOOK6   MVI   0(R5),0             BOOK EXPRESSION - FIRST BYTE ZERO            
         LA    R7,22(R4)           POINT TO YEAR/BTYPE SLOT                     
         ZIC   R1,1(R4)            GET ITS LENGTH                               
         LTR   R1,R1               MUST BE SPECIFIED                            
         BZ    GETERR                                                           
         AR    R7,R1                                                            
         BCTR  R7,0                                                             
         CLI   0(R7),C')'          CHECK FOR A BOOK TYPE                        
         BNE   RBOOK10                                                          
         BCTR  R7,0                                                             
         MVC   3(1,R5),0(R7)                                                    
         SH    R1,=H'3'            ADJUST FOR BTYPE SUFFIX                      
         LR    RF,R1                                                            
         LA    R7,22(R4)                                                        
RBOOK8   CLI   0(R7),C'0'          CHECK FOR A YEAR ENTERED                     
         BL    GETERR              ERROR IF NOT NUMERIC                         
         CLI   0(R7),C'9'                                                       
         BH    GETERR                                                           
         LA    R7,1(R7)                                                         
         BCT   R1,RBOOK8                                                        
         LR    R1,RF               CONVERT YEAR TO BINARY                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,22(0,R4)                                                     
         CVB   R1,DUB                                                           
         B     *+8                                                              
*                                                                               
RBOOK10  L     R1,8(R4)            YEAR COMES FROM SECOND EXPRESSION            
         LTR   R1,R1                                                            
         BZ    GETERR                                                           
         CH    R1,=H'1900'         ALLOW BIG YEARS                              
         BL    *+12                                                             
         SH    R1,=H'1900'                                                      
         B     RBOOK15             YEARS LOWER THAN 27, +100                    
         CH    R1,=H'27'                                                        
         BH    RBOOK15                                                          
         AHI   R1,100                                                           
RBOOK15  STC   R1,1(R5)                                                         
         MVC   2(1,R5),7(RE)       'MONTH' FROM SEASON TABLE                    
         SPACE 1                                                                
         LA    R5,4(R5)                                                         
         LA    R4,32(R4)                                                        
         BCT   R0,RBOOK2                                                        
         B     XIT                                                              
         SPACE 1                                                                
RBLIST   DC    C'WINTER ',AL1(02)  ARB/BIRCH BOOKS                              
         DC    C'SPRING ',AL1(05)                                               
         DC    C'SUMMER ',AL1(07)                                               
         DC    C'AUTUMN ',AL1(11)                                               
         DC    C'FALL   ',AL1(11)                                               
         DC    C'JAN    ',AL1(01)                                               
         DC    C'FEB    ',AL1(02)                                               
         DC    C'MAR    ',AL1(03)                                               
         DC    C'APR    ',AL1(04)                                               
         DC    C'MAY    ',AL1(05)                                               
         DC    C'JUNE   ',AL1(06)                                               
         DC    C'JULY   ',AL1(07)                                               
         DC    C'AUG    ',AL1(08)                                               
         DC    C'SEP    ',AL1(09)                                               
         DC    C'OCT    ',AL1(10)                                               
         DC    C'NOV    ',AL1(11)                                               
         DC    C'DEC    ',AL1(12)                                               
         DC    C'WINSPR ',AL1(05)                                               
         DC    C'FWS    ',AL1(05)                                               
         DC    C'SUMFAL ',AL1(11)                                               
         DC    X'FF'                                                            
         SPACE 1                                                                
RBLISTM  DC    C'WINTER ',AL1(01)  BBM BOOKS                                    
         DC    C'SPRING ',AL1(05)                                               
         DC    C'SUMMER ',AL1(07)                                               
         DC    C'AUTUMN ',AL1(11)                                               
         DC    C'FALL   ',AL1(11)                                               
         DC    C'JAN    ',AL1(01)                                               
         DC    C'MAY    ',AL1(05)                                               
         DC    C'JULY   ',AL1(07)                                               
         DC    C'NOV    ',AL1(11)                                               
         DC    X'FF'                                                            
RBLISTR  DC    C'WINTER ',AL1(02)  RADAR BOOKS                                  
         DC    C'SPRING ',AL1(05)                                               
         DC    C'SUMMER ',AL1(07)                                               
         DC    C'AUTUMN ',AL1(11)                                               
         DC    C'FALL   ',AL1(11)                                               
         DC    C'FEB    ',AL1(02)                                               
         DC    C'MAY    ',AL1(05)                                               
         DC    C'JULY   ',AL1(07)                                               
         DC    C'NOV    ',AL1(11)                                               
         DC    C'DEC    ',AL1(12)                                               
         DC    X'FF'                                                            
         EJECT                                                                  
*-----------------------------------------------------------------              
* GET MARKET NUMBER FOR CITY CODE                                               
*  INPUT: R5 = ADDRESS OF TABLE                                                 
*                0(R5) = MEDIA TYPE                                             
*                1(R5) = SOURCE TYPE                                            
*                2(R5) = BOOK TYPE                                              
*                3(R5) = 3 CHAR CITY CODE                                       
* OUTPUT: R1 = 0     ERROR                                                      
*            <> 0    MARKET NUMBER                                              
*-----------------------------------------------------------------              
CITYMRKT XC    TEMPKEY,TEMPKEY     TRY TO GET MARKET FROM ALPHA CODES           
         LA    R2,TEMPKEY                                                       
         USING CTDMREC,R2                                                       
         MVI   CTDMKTYP,CTDMKTEQ                                                
         MVI   CTDMKTY2,CTDMKT2E                                                
         MVI   CTDMKMED,C'R'                                                    
         MVC   CTDMKSRC,1(R5)                                                   
         MVC   CTDMKMKT,3(R5)                                                   
         MVC   CTDMKBKT,2(R5)                                                   
*                                                                               
         LA    R1,PPMBTAB                                                       
CM05     CLI   0(R1),X'FF'                                                      
         BE    CM10                                                             
         CLC   CTDMKBKT,1(R1)      ONE OF THE ALTERNATE BOOKTYPE?               
         BNE   *+14                                                             
         MVC   CTDMKBKT,0(R1)      MOVE IN THE PRIMARY ONE                      
         B     CM10                                                             
         LA    R1,L'PPMBTAB(R1)                                                 
         B     CM05                                                             
*                                                                               
CM10     CLI   CTDMKBKT,0                                                       
         BNZ   *+8                                                              
         MVI   CTDMKBKT,X'FF'                                                   
         XC    KEYSAVE,KEYSAVE                                                  
         MVC   KEYSAVE(23),TEMPKEY                                              
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI  '),=C'CTFILE  ',TEMPKEY,     +        
               TEMPKEY,0                                                        
         CLC   TEMPKEY(23),KEYSAVE                                              
         BNE   XITCMKTE            CAN'T FIND IT                                
         SR    R1,R1                                                            
         ICM   R1,3,CTDMKNUM                                                    
         B     XITCMKT                                                          
XITCMKTE SR    R1,R1                                                            
XITCMKT  LTR   R1,R1                                                            
         XIT1  REGS=(R1)                                                        
*                                                                               
PPMBTAB  DS    0XL2                PPM BOOKTYPE TABLE                           
         DC    X'00',C'D'                                                       
         DC    X'00',C'P'                                                       
         DC    C'B',C'K'                                                        
         DC    C'B',C'E'                                                        
         DC    C'H',C'S'                                                        
         DC    C'H',C'I'                                                        
         DC    X'FF'                                                            
*                                                                               
         EJECT                                                                  
*-----------------------------------------------------------------              
* GET ALPHA CODE FOR MARKET NUMBER                                              
*  INPUT: R5 = ADDRESS OF TABLE                                                 
*                0(R5) = MEDIA TYPE                                             
*                1(R5) = SOURCE TYPE                                            
*                2(R5) = BOOK TYPE                                              
*                3(R5) = MARKET NUMBER                                          
* OUTPUT: 5(R5) = ADDRESS OF MARKET CODE                                        
*-----------------------------------------------------------------              
NUM2CODE XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING CTDMREC,R2                                                       
         MVC   5(3,R5),=C'   '                                                  
         MVI   CTDMKTYP,CTDMKTEQ   SET UP KEY                                   
         MVI   CTDMKTY2,CTDMKT2E                                                
         MVC   CTDMKMED,0(R5)                                                   
         MVC   CTDMKSRC,1(R5)                                                   
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI  ',=C'CTFILE  ',KEYSAVE,KEY,0             
NUM2CD5  CLC   KEY(19),KEYSAVE                                                  
         BNE   XIT                                                              
         CLC   CTDMKNUM,3(R5)                                                   
         BNE   NUM2CD8                                                          
         MVC   5(3,R5),KEY+19                                                   
         B     XIT                                                              
NUM2CD8  GOTO1 DATAMGR,DMCB,=C'DMRSEQ  ',=C'CTFILE  ',KEYSAVE,KEY,0             
         B     NUM2CD5                                                          
         EJECT                                                                  
*-----------------------------------------------------------------              
* PARSER FOR A FIELDS                                                           
*  INPUT: R1 = ADDRESS OF DMCB                                                  
*                0(R5) = ADDRESS OF FIELDS AND # OF FIELDS                      
*                        1ST BYTE IS # OF FIELDS                                
*                        NEXT 3 IS ADDRESS OF STARTING FIELD                    
*                4(R5) = ADDRESS OF STOP CHAR LIST                              
*                8(R5) = ADDRESS OF MAX OUTPUT LENGTH (1 BYTE)                  
*                        ADDRESS OF OUTPUT AREA       (3 BYTES)                 
* OUTPUT: R5 = ADDRESS OF OUTPUT, WHICH IS BROKEN INTO ELEMENTS                 
*                0(ELEM) = STOP CHAR FOR ELEMENT                                
*                          X'00' FIELD > MAX OUTPUT LENGTH, ERROR               
*                          X'FF' END OF LIST, NO ERRORS                         
*                1(ELEM) = LENGTH OF ELEMENT'S DATA                             
*                2(ELEM) = ADDRESS DISPLACEMENT OF TWA FOR ELEMENT              
*                4(ELEM) = DISP INTO FIELD                                      
*                5(ELEM) = DATA                                                 
*-----------------------------------------------------------------              
MINIPAR  DS    0H                                                               
         L     R2,0(R1)            START GETTING PARAMETERS                     
         L     R3,4(R1)            ADDRESS OF STOP CHAR LIST                    
         ZIC   R4,8(R1)            GET MAX LENGTH OF OUTPUT                     
         SH    R4,=H'2'            SAVE 2 BYTES FOR END OF LIST                 
         STC   R4,MAXOUTLN                                                      
         L     R5,8(R1)            ADDRESS OF OUTPUT AREA                       
         LA    R5,0(R5)            CLEAR HIGH ORDER BYTE                        
         XC    CHROUTLN,CHROUTLN                                                
         MVI   ENDFIELD,C'N'       FIELD WASN'T ENDED                           
         MVI   NEWADDR,C'Y'                                                     
MORETABL L     R4,0(R2)            ADDRESS OF FIRST FIELD HEADER                
         LA    R4,0(R4)                                                         
         LTR   R4,R4               SEE IF NO MORE TABLE ENTRIES                 
         BNZ   TABLEOKB            USE THE TABLE ENTRY                          
*                                                                               
         MVI   0(R5),X'FF'         END OF LIST                                  
         MVI   1(R5),X'0'          WE ARE FINISHED!                             
         B     XIT                                                              
*                                                                               
TABLEOKB LR    R7,R4               CALCULATE DISP OF FIELD                      
         S     R7,ATWA               FROM TWA                                   
         STCM  R7,3,OLDADDR        SAVE IT FOR LATER                            
         STCM  R7,3,FLDADDR        SAVE IT FOR LATER                            
*                                                                               
TABLEOK  ZIC   R1,0(R2)            # OF FIELDS TO BE PROCESSED                  
         LTR   R1,R1                                                            
         BZ    NEXTTABL            NEXT TABLE ENTRY                             
         STC   R1,NUMFIELD                                                      
         LA    RF,8(R4)            TAIL, FIRST CHAR IN FIELD                    
         ZIC   R1,5(R4)            LENGTH OF INPUT IN FIELD                     
         LA    RE,8(R1,R4)         ADDRESS OF LAST CHAR+1                       
         LR    R1,RE                                                            
         BCTR  R1,0                LAST CHAR ADDRESS                            
         CLI   0(R1),C','          IS IT A ','                                  
         BNE   TABLEOK1                                                         
         BCTR  RE,0                ONE LESS FOR ,                               
TABLEOK1 XC    TMPBUFF,TMPBUFF     INITIALIZE SCRATCH SPACE                     
         XC    DATALEN,DATALEN     INITIALIZE DATA LENGTH                       
STPAGAIN CR    RF,RE               SEE IF PASSED FIELD                          
         BL    BSTOPLP             NO GO TO STOP CHAR LOOP                      
         B     NXTTAILA            GET NEXT FIELD                               
*                                                                               
* IS CURRENT CHARACTER A STOP CHARACTER?                                        
*                                                                               
BSTOPLP  CLI   NEWADDR,C'Y'        ENTRY ADDR DISP NEEDED?                      
         BNE   BSTOPLPA            NO, PROCEED                                  
         MVI   NEWADDR,C'N'        YES, CLEAR IT                                
         LA    R0,8(R4)            GET ADDR OF FIRST CHAR                       
         LR    R1,RF               GET CURRENT POSITION                         
         SR    R1,R0               FIND DISP OF CURRENT-FIRST                   
         STC   R1,FLDDISP                                                       
BSTOPLPA LR    R7,R3                                                            
STPCHRLP CLI   0(R7),X'FF'         END OF STOP CHAR LIST?                       
         BE    NOSTPCHR            YES, CHAR NOT A STOP CHAR                    
         CLC   0(1,RF),0(R7)       CHECK IF CHAR IN STOP CHAR LIST              
         BE    ISSTPCHR            YES, IT IS A STOP CHAR                       
         LA    R7,1(R7)            NEXT STOP CHAR IN LIST                       
         B     STPCHRLP                                                         
*                                                                               
ISSTPCHR ZIC   R6,DATALEN                                                       
         ZIC   R7,CHROUTLN                                                      
         LA    R7,2(R7)            INCLUDE STOP CHAR & DATALEN                  
         AR    R7,R6                                                            
         ZIC   R6,MAXOUTLN                                                      
         CR    R7,R6               DID WE PASS MAXIMUM OUTPUT LENGTH?           
         BNE   OUTELEM             NO, SEND OUT TMPBUFF                         
         MVC   0(2,R5),=X'0000'    SEND OUT ERROR                               
         B     XIT                                                              
OUTELEM  MVC   0(1,R5),0(RF)       SEND STOP CHAR                               
         CLI   ENDFIELD,C'N'       NO SPECIAL END FIELD?                        
         BE    *+8                                                              
         MVI   0(R5),C','          END FIELD SHOULD BE A COMMA                  
         MVC   1(1,R5),DATALEN     SEND OUT LENGTH OF DATA                      
         SR    R1,R1                                                            
         MVC   2(2,R5),OLDADDR     FIELD DISP                                   
         MVC   4(1,R5),FLDDISP     DISP INTO FIELD                              
         MVC   OLDADDR,FLDADDR     FIELD DISP                                   
         ZIC   R1,DATALEN                                                       
         LTR   R1,R1               DON'T MVC IF R1=0                            
         BZ    OUTELEMA                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   5(0,R5),TMPBUFF     SEND OUT DATA                                
OUTELEMA STC   R7,CHROUTLN         UPDATE OUTPUT LENGTH                         
*-------------------                                                            
         LR    R7,R4               CALCULATE DISP OF FIELD                      
         S     R7,ATWA               FROM TWA                                   
         STCM  R7,3,FLDADDR        SAVE IT FOR LATER                            
*-------------------                                                            
         ZIC   R7,DATALEN                                                       
         LA    R5,5(R7,R5)         NEXT CHAR POSITION                           
         MVI   NEWADDR,C'Y'        NEW DISP ADDR NEEDED                         
         XC    DATALEN,DATALEN     INITIALIZE DATA LENGTH                       
         XC    TMPBUFF,TMPBUFF       AND SCRATCH SPACE                          
         CLI   ENDFIELD,C'F'       FIELDS FINISHED?                             
         BE    NEXTTABL            GO TO TABLE CALCS                            
         B     NEXTTAIL                                                         
*                                                                               
NOSTPCHR CLI   ENDFIELD,C'Y'       WAS FIELD ENDED?                             
         BNE   DOTMPSTF            NO, DO TEMPORARY CALCULATIONS                
         CLI   TMPBUFF,C'+'        IF IT STARTS WITH A '+'                      
         BE    DOTMPSTF              CONSIDER IT AS A CONTINUATION              
         BCTR  RF,0                DECREMENT TAIL PTR                           
*                                    BECAUSE WE WANT IT HERE AGAIN              
         B     ISSTPCHR            CURRENT ELEMENT NOT A COMBO                  
*                                                                               
DOTMPSTF ZIC   R6,DATALEN                                                       
         LA    R7,TMPBUFF                                                       
         LA    R7,0(R6,R7)         GET LAST CHAR ADDRESS                        
         MVC   0(1,R7),0(RF)       SAVE CHAR IN TMPBUFF                         
         LR    R7,R6               GET LENGTH OF DATA                           
         LA    R7,1(R7)            INCREMENT IT                                 
         STC   R7,DATALEN          SAVE IT                                      
*                                                                               
NEXTTAIL MVI   ENDFIELD,C'N'       NOT END OF FIELD                             
         LA    RF,1(RF)            CALCULATE NEXT TAIL POSITION                 
         CR    RF,RE               SEE IF TAIL PASSED INPUT                     
         BL    BSTOPLP             NOPE, USE IT                                 
         MVI   ENDFIELD,C'Y'       END OF FIELD                                 
NXTTAILA ZIC   R1,NUMFIELD         CHECK IF NUMBER FIELDS OVER                  
         BCTR  R1,0                                                             
         STC   R1,NUMFIELD                                                      
         LTR   R1,R1               SEE IF NUMFIELDS IS 0                        
         BZ    FINIFLD             FIELDS FINISHED DO NEXT ONE                  
*                                                                               
PSKIPSA  ZIC   R1,0(R4)            LENGTH OF THIS FIELD                         
         AR    R4,R1               NEXT ONE                                     
         TM    1(R4),X'20'         SKIP PROTECTED FIELDS                        
         BO    PSKIPSA                                                          
*                                                                               
PSKIPS   LR    R7,R4               CALCULATE DISP OF FIELD                      
         S     R7,ATWA               FROM TWA                                   
         STCM  R7,3,FLDADDR        SAVE IT FOR LATER                            
*                                                                               
         LA    RF,8(R4)            TAIL IS NOW BEGINNING OF DATA                
         ZIC   R1,5(R4)                                                         
         LA    RE,8(R1,R4)         ADDRESS OF LAST CHAR+1                       
         B     STPAGAIN                                                         
*                                                                               
FINIFLD  CLI   DATALEN,0           IS ELEMENT BLANK?                            
         BZ    NEXTTABL            YES, USE NEXT TABLE ENTRY                    
         MVI   ENDFIELD,C'F'                                                    
         B     ISSTPCHR                                                         
NEXTTABL MVI   ENDFIELD,C'N'                                                    
         LA    R2,4(R2)            NEXT TABLE ENTRY                             
         B     MORETABL                                                         
*                                                                               
         B     XIT                                                              
         SPACE 1                                                                
*NEWADDR  DC    C'Y'                NEED NEW ENTRY ADDRESS                      
*FLDADDR  DC    XL2'0000'           ENTRY DISPLACEMENT ADDRESS                  
*OLDADDR  DC    XL2'0000'           ENTRY DISPLACEMENT ADDRESS                  
*FLDDISP  DC    X'0'                DISPLACEMENT INTO FIELD                     
*NUMFIELD DC    X'0'                # OF FIELDS TO BE PROCESSED                 
*MAXOUTLN DC    X'0'                MAXIMUM LENGTH FOR OUTPUT                   
*ENDFIELD DC    C'N'                FIELD END INDICATOR                         
*CHROUTLN DC    X'0'                CURRENT LENGTH OF OUTPUT                    
*DATALEN  DC    X'0'                LENGTH OF DATA                              
*TMPBUFF  DC    CL255' '            TEMPORARY STORAGE FOR DATA                  
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
* VALIDATE DEMO EXPRESSIONS                                                     
         SPACE 1                                                                
VALDEM   DS    0H                                                               
         CLI   5(R2),0                                                          
         BE    XIT                                                              
* SCAN FOR SHORT DEMOS                                                          
         LA    R0,8                8 IS ORDINARY LIMIT                          
         CLI   NDEMOS,8            UNLESS NDEMOS IS SET TO MORE                 
         BNH   *+8                                                              
         IC    R0,NDEMOS                                                        
         MVI   SCANLEN,32                                                       
         XC    BLOCK(256),BLOCK                                                 
         GOTO1 SCANNER,PARAS,(R2),((R0),BLOCK)                                  
*                                                                               
         XC    XTRABLK(256),XTRABLK     MAKE SURE IT IS NULLED                  
         XC    DEMCNT,DEMCNT                                                    
         LA    R6,XTRABLK                                                       
         LA    R4,BLOCK                                                         
AGAIN    CLI   0(R4),X'0'          FINISHED?                                    
         BZ    QUITTHIS                                                         
         CLI   0(R4),X'4'          # CHARS > 4?                                 
         BH    STRNGOK                                                          
         CLI   0(R4),X'1'          SHORTHAND DEMO?                              
         BNE   STRNGOK                                                          
         CLI   1(R4),X'0'          MAYBE A M=???                                
         BH    STRNGOK             YES, USE IT                                  
         CLI   12(R4),C'M'         SHORTHAND DEMO = M,W OR L                    
         BE    MWL_B1                                                           
         CLI   12(R4),C'W'                                                      
         BE    MWL_B1                                                           
         CLI   12(R4),C'L'                                                      
         BE    MWL_B1                                                           
         B     STRNGOK                                                          
MWL_B1   LA    R8,1                INITIALIZE COUNTER                           
         ST    R8,COUNTSHT           OF SHORTHAND DEMO LETTERS                  
         ST    R4,FIRSTLET         STORE ADDR OF FIRST LETTER                   
NEXTLOOP LA    R4,32(R4)                                                        
         CLI   0(R4),1                                                          
         BNE   PUTTHEM                                                          
         CLI   12(R4),C'M'         SHORTHAND DEMO = M,W OR L                    
         BE    MWL_B2                                                           
         CLI   12(R4),C'W'                                                      
         BE    MWL_B2                                                           
         CLI   12(R4),C'L'                                                      
         BE    MWL_B2                                                           
         B     PUTTHEM                                                          
MWL_B2   L     R8,COUNTSHT                                                      
         LA    R8,1(R8)                                                         
         ST    R8,COUNTSHT                                                      
         B     NEXTLOOP                                                         
PUTTHEM  ST    R4,STOPADD                                                       
         L     R8,COUNTSHT                                                      
         L     R5,FIRSTLET                                                      
LP2      L     R4,STOPADD                                                       
LP3      CLI   0(R4),X'0'                                                       
         BE    STOP                                                             
         CLI   12(R4),C'M'                                                      
         BE    STOP                                                             
         CLI   12(R4),C'W'                                                      
         BE    STOP                                                             
         CLI   12(R4),C'L'                                                      
         BE    STOP                                                             
         MVC   0(32,R6),0(R4)                                                   
         ZIC   R7,0(R4)                                                         
         BCTR  R7,0                                                             
         EX    R7,*+8                                                           
         B     *+10                                                             
         MVC   13(0,R6),12(R4)                                                  
         MVC   12(1,R6),12(R5)                                                  
         XC    1(11,R6),1(R6)      CLEAR OLD DATA                               
         LA    R7,2(R7)                                                         
         STC   R7,0(R6)                                                         
         LA    R4,32(R4)                                                        
         LA    R6,32(R6)                                                        
         LH    R3,DEMCNT                                                        
         LA    R3,1(R3)                                                         
         STH   R3,DEMCNT                                                        
         B     LP3                                                              
STRNGOK  MVC   0(32,R6),0(R4)                                                   
         LA    R4,32(R4)                                                        
         LA    R6,32(R6)                                                        
         LH    R3,DEMCNT                                                        
         LA    R3,1(R3)                                                         
         STH   R3,DEMCNT                                                        
         B     AGAIN                                                            
STOP     LA    R5,32(R5)                                                        
         BCT   R8,LP2                                                           
         B     AGAIN                                                            
QUITTHIS MOVE  (BLOCK,480),XTRABLK                                              
         LH    R3,DEMCNT                                                        
         STC   R3,PARAS+4                                                       
*                                                                               
         LA    R4,BLOCK                                                         
         LA    R5,DEMOS                                                         
         ZIC   R0,PARAS+4                                                       
         LTR   R0,R0                                                            
         BZ    BADDEM                                                           
         STC   R0,NDEMOS                                                        
         SPACE 1                                                                
         CLC   12(2,R4),=C'M '     MENU OPTION                                  
         BE    VDEM10                                                           
         CLC   12(2,R4),=C'L '     LIST OPTION                                  
         BNE   VDEM6                                                            
         XC    WORK,WORK                                                        
         ZIC   R1,1(R4)                                                         
         STC   R1,WORK+5                                                        
         MVC   WORK+8(10),22(R4)                                                
         BAS   RE,VONEDEM                                                       
         BNE   BADDEM                                                           
         LA    R1,TYPLIST                                                       
         LA    R6,DEMOS                                                         
         LA    R0,6                                                             
         MVI   NDEMOS,6                                                         
         CLI   DBSELMED,C'C'       CANADIAN LIST                                
         BNE   VDEM2                                                            
         LA    R1,CANLIST                                                       
         LA    R0,3                                                             
         MVI   NDEMOS,3                                                         
         SPACE 1                                                                
VDEM2    MVC   0(3,R6),WORK                                                     
         MVC   1(1,R6),0(R1)                                                    
         LA    R1,1(R1)                                                         
         LA    R6,3(R6)                                                         
         BCT   R0,VDEM2                                                         
         CLI   BLOCK+32,0          SECOND CANADIAN LIST                         
         BE    XIT                                                              
         CLI   DBSELMED,C'C'                                                    
         BNE   BADDEM                                                           
         XC    WORK,WORK                                                        
         LA    R6,BLOCK+32                                                      
         ZIC   R1,0(R6)                                                         
         STC   R1,WORK+5                                                        
         MVC   WORK+8(10),12(R6)                                                
         BAS   RE,VONEDEM                                                       
         BNE   BADDEM                                                           
         LA    R6,DEMOS+9                                                       
         MVI   NDEMOS,6                                                         
         LA    R1,CANLIST                                                       
         LA    R0,3                                                             
         SPACE 1                                                                
VDEM3    MVC   0(3,R6),WORK                                                     
         MVC   1(1,R6),0(R1)                                                    
         LA    R1,1(R1)                                                         
         LA    R6,3(R6)                                                         
         BCT   R0,VDEM3                                                         
         B     XIT                                                              
         SPACE 1                                                                
TYPLIST  DC    C'RTPXSQ'                                                        
CANLIST  DC    C'REI'                                                           
         SPACE 1                                                                
VDEM6    MVC   0(3,R5),=X'20C9FF'  SPECIAL COST HANDLING                        
         CLC   12(4,R4),=C'COST'                                                
         BE    VDEMEND                                                          
         CLC   12(2,R4),=C'$ '                                                  
         BE    VDEMEND                                                          
         XC    WORK,WORK           BUILD A SINGLE HEADER                        
         ZIC   R1,0(R4)                                                         
         MVC   WORK+8(10),12(R4)                                                
         CLI   12(R4),C'$'                                                      
         BNE   VDEM8                                                            
         BCTR  R1,0                                                             
         MVC   WORK+8(9),WORK+9                                                 
         SPACE 1                                                                
VDEM8    STC   R1,WORK+5                                                        
         BAS   RE,VONEDEM                                                       
         BNE   BADDEM                                                           
         MVC   0(3,R5),WORK                                                     
         CLI   12(R4),C'$'                                                      
         BNE   *+8                                                              
         OI    0(R5),X'20'                                                      
         SPACE 1                                                                
VDEMEND  LA    R5,3(R5)                                                         
         LA    R4,32(R4)                                                        
         BCT   R0,VDEM6                                                         
         B     XIT                                                              
         SPACE 1                                                                
BADDEM   MVI   ERROR,INVDEMO                                                    
         B     CURSERR                                                          
         SPACE 1                                                                
VONEDEM  NTR1                                                                   
         MVI   WORK,18             SET HEADER LENGTH                            
         LA    R5,IO                  PHONY EST HEADER                          
         GOTO1 DEMOVAL,PARAS,WORK,(1,WORK+20),(C'S',DBLOCK),(R5)                
         MVC   WORK(3),WORK+20                                                  
         CLI   4(R1),0                                                          
         BE    NO                                                               
         B     YES                                                              
         SPACE 1                                                                
*COUNTSHT DC    F'0'                                                            
*DEMCNT   DC    H'0'                                                            
*FIRSTLET DC    F'0'                                                            
*STOPADD  DC    F'0'                                                            
*XTRABLK  DC    480X'0'                                                         
         EJECT                                                                  
* VALIDATE DEMO LIST NAME *                                                     
         SPACE 1                                                                
VDEM10   DS    0H                                                               
         MVI   ERROR,BADMENU                                                    
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D26'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         MVC   KEY+3(4),22(R4)                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   CURSERR                                                          
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,5                                                         
         LA    R7,10               SET MAX COUNTER                              
         BAS   RE,GETEL                                                         
         BNE   CURSERR                                                          
         B     VDEM14                                                           
*                                                                               
VDEM12   BAS   RE,NEXTEL                                                        
         BNE   VDEM16                                                           
VDEM14   MVC   0(3,R5),2(R6)       MOVE DEMO VALUE                              
         LA    R5,3(R5)                                                         
         BCT   R7,VDEM12                                                        
*                                                                               
VDEM16   LA    R0,10                                                            
         SR    R0,R7                                                            
         STC   R0,NDEMOS                                                        
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
* VALIDATE CATEGORIES                                                           
         SPACE 3                                                                
*              INPUT COMBINATIONS  AREAS  TSA MSA ADI                           
*                                  DATA   IMP RTG CUME CUM% XCUM                
*                                         PUT TOT AWAY COMBO                    
*              OUTPUTS             DEMOS  NDEMOS  CATTITS                       
         SPACE 1                                                                
VALCATS  MVI   SCANLEN,32                                                       
         GOTO1 ANY                                                              
         GOTO1 SCANNER,PARAS,(R2),(10,BLOCK)                                    
         LA    R4,BLOCK                                                         
         LA    R5,DEMOS                                                         
         LA    R6,CATTITS                                                       
         MVI   NCATS,0                                                          
         ZIC   R0,PARAS+4                                                       
         LTR   R0,R0                                                            
         BZ    BADCAT                                                           
         SPACE 1                                                                
VALCAT2  L     R3,=A(CATLIST)                                                   
         A     R3,RELO                                                          
         ZIC   R1,0(R4)                                                         
         BCTR  R1,0                                                             
         SPACE 1                                                                
VALCAT4  EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R3),12(R4)                                                   
         BE    VALCAT6                                                          
         LA    R3,12(R3)                                                        
         CLI   0(R3),X'FF'                                                      
         BNE   VALCAT4                                                          
         B     BADCAT                                                           
         SPACE 1                                                                
VALCAT6  CLI   6(R3),3             AREA EXPRESSION                              
         BH    VALCAT8                                                          
         CLI   DBSELMED,C'R'       FOR CANADIAN, BYPASS TESTS                   
         BNE   *+12                                                             
         CLI   DBSELSRC,C'M'       CANADIAN: MED=R, SRC=M                       
         BE    VALCAT7             YES                                          
         CLI   6(R3),2             TSA?                                         
         BE    *+12                                                             
         CLI   6(R3),3             ADI?                                         
         BNE   *+12                                                             
         BAS   RE,VALARB           NEW ARB CONDITIONS FOR TSA/ADI               
         BNZ   BADCAT2             TSA/ADI INVALID FOR BOOK                     
*                                                                               
VALCAT7  MVC   0(1,R5),6(R3)       (FIRST BYTE OF DEMO)                         
         MVC   0(1,R6),12(R4)      (FIRST LETTER FOR CATEGORY TITLE)            
         CLI   DBSELSRC,C'N'       IF THIS IS BIRCH                             
         BNE   VALCNXT                                                          
         CLI   6(R3),1             IT MUST BE MSA                               
         BNE   BADCAT                                                           
         B     VALCNXT                                                          
         SPACE 1                                                                
VALCAT8  MVC   1(1,R5),6(R3)       CATEGORY EXPRESSION                          
         CLI   0(R5),0             MUST BE AN AREA BY NOW                       
*        BE    BADCAT                                                           
         CLI   2(R5),0             IS THERE AN AGE/SEX HERE                     
         BNE   *+10                                                             
         MVC   2(1,R5),DEMOS+2     NO SO USE FROM FIRST DEMO                    
         MVC   3(1,R5),0(R5)       PASS ON THE AREA CODE                        
         MVC   1(4,R6),8(R3)       PICK UP CATEGORY TITLE                       
         MVC   5(1,R6),0(R6)       COPY AREA LETTER TO NEXT TITLE               
         CLI   4(R6),C' '          IF LAST CHARACTER IS A SPACE                 
         BNE   VALCAT9                                                          
         MVI   WORK,C' '           SHUFFLE TO RIGHT                             
         MVC   WORK+1(4),0(R6)                                                  
         MVC   0(5,R6),WORK                                                     
         SPACE 1                                                                
VALCAT9  CLI   0(R5),2             SOME TSA CATEGORIES ARE SPECIAL              
         BNE   VALCAT10                                                         
         CLI   1(R2),C'S'          SHARE IS X                                   
         BNE   *+8                                                              
         MVI   1(R2),C'X'                                                       
         CLI   1(R2),C'Q'          TOTAL (Q) IS INVALID                         
         BE    BADCAT                                                           
         SPACE 1                                                                
VALCAT10 LA    R5,3(R5)                                                         
         LA    R6,5(R6)                                                         
         AI    NCATS,1             BUMP N'DEMOS                                 
         CLC   NCATS,MAX           MAX 4                                        
         BH    BADCAT                                                           
         SPACE 1                                                                
VALCNXT  LA    R4,32(R4)                                                        
         BCT   R0,VALCAT2                                                       
         CLI   NCATS,0             MUST BE AT LEAST 1 CATEGORY                  
         BE    BADCAT                                                           
         CLC   NCATS,NDEMOS                                                     
         BE    VALCEND                                                          
         BL    VALCPRO                                                          
         MVC   NDEMOS,NCATS                                                     
         B     VALCEND                                                          
         SPACE 1                                                                
VALCPRO  ZIC   R0,NDEMOS           LESS CATS THAN DEMOS                         
         ZIC   R1,NCATS            SO NEED TO PROJECT CATEGORIES                
         SR    R0,R1                                                            
         SPACE 1                                                                
VALCPRO2 MVC   0(2,R5),DEMOS       MOVE CATEGORY FROM FIRST                     
         LA    R5,3(R5)                                                         
         BCT   R0,VALCPRO2                                                      
         SPACE 1                                                                
VALCEND  MVI   0(R5),X'FF'                                                      
         B     XIT                                                              
         SPACE 1                                                                
BADCAT   MVI   ERROR,INVCAT                                                     
         B     CURSERR                                                          
BADCAT2  MVI   ERROR,BADCATBK      ADI/TSA INVALID FOR BOOK                     
         B     CURSERR                                                          
         EJECT                                                                  
*                                                                               
*VALARB -  IF (BOOK >=NOV93) THEN                                               
*            IF (BKTYPE=C'N') THEN                                              
*               ACCEPT TSA & ADI !! (DISREGARD OTHER CHECK)                     
*            IF (BKTYPE<>0) OR ((BKTYPE=0) & NOT(SPR OR FALL) THEN              
*                 TSA AND ADI INVALID  CC=NE                                    
*          ELSE   CC=EQUAL                                                      
*                                                                               
VALARB   DS    0H                                                               
         NTR1                                                                   
         ZIC   R0,NBOOKS           PARSE THRU ALL BOOKS LISTED                  
         LA    R5,BOOKS                                                         
VALARB5  CLI   1(R5),X'5D'         93+                                          
         BL    VALARBN             -93, NO TESTS NEEDED, NEXT BK                
         BH    *+12                AFTER 93                                     
         CLI   2(R5),11            NOV93?                                       
         BNE   VALARBN             BEFORE NOV93- BK OKAY                        
         CLI   3(R5),C'N'          IF BKTYPE=N, CATAG OKAY                      
         BE    VALARBN                                                          
         CLI   3(R5),0             BKTYPE=0?                                    
         BNE   NO                  INVALID BOOK/CATAGORY                        
         CLI   2(R5),5             BKTYPE=0  &  BOOK=MAY?                       
         BE    VALARBN             YES, BOOK OKAY                               
         CLI   2(R5),11            BKTYPE=0 & BOOK=NOV?                         
         BNE   NO                  NO - INVALID                                 
VALARBN  LA    R5,4(R5)                                                         
         BCT   R0,VALARB5          PROCESS NEXT BOOK                            
         B     YES                 DONE WITH BOOKS, ALL CHECKS OUT              
         EJECT                                                                  
*                                                                               
* VALIDATE DAY *                                                                
         SPACE 1                                                                
VALDAY   DS    0H                                                               
         GOTO1 ANY                                                              
         L     R5,=A(DAYTBL)                                                    
         A     R5,RELO                                                          
         SR    R6,R6                                                            
         SPACE 1                                                                
VALDAY2  MVC   ACTUAL(1),7(R5)     PRESET DAY NUMBER                            
         ZIC   R6,5(R2)            SET VARIABLE LENGTH                          
         BCTR  R6,0                                                             
         EX    R6,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R5),8(R2)       *EXECUTED*                                   
         BE    XIT                                                              
         LA    R5,8(R5)                                                         
*        LA    R6,1(R6)                                                         
         CLI   0(R5),X'FF'                                                      
         BNE   VALDAY2                                                          
         SPACE 1                                                                
BADDAY   MVI   ERROR,INVDAYTM                                                   
         B     GETERR                                                           
         EJECT                                                                  
* VALIDATE DAY/TIME EXPRESSIONS *                                               
* INPUT  P1  =  MAXIMUM NUMBER OF DAY/TIMES                                     
         SPACE 1                                                                
VALDYTM  L     R6,0(R1)                                                         
         MVI   NDAYTMS,0                                                        
         XC    DAYTMLST,DAYTMLST                                                
         LA    R5,DAYTMLST                                                      
         SPACE 1                                                                
         GOTO1 ANY                 1 DAY REQUIRED                               
         SPACE 1                                                                
DT2      CLI   5(R2),0                                                          
         BE    XIT                                                              
         MVI   0(R5),X'FF'         CHECK FOR 'ALL'                              
         CLC   8(3,R2),=C'ALL'                                                  
         BE    DT6                                                              
         L     RE,=A(DAYLIST)      LOOK UP DAY LIST                             
         A     RE,RELO                                                          
         SR    RF,RF                                                            
         SR    R4,R4                                                            
         SPACE 1                                                                
DT4      STC   RF,0(R5)            RETURN DAY NUMBER                            
         IC    R4,5(R2)                                                         
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),0(RE)       IF DAY MATCHES                               
         BE    DT6                                                              
         LA    RE,8(RE)                                                         
         IC    RF,7(RE)                                                         
         CLI   0(RE),X'FF'                                                      
         BNE   DT4                                                              
         MVI   ERROR,INVDAYTM                                                   
         B     GETERR                                                           
         SPACE 1                                                                
DT6      BAS   RE,BUMP             LOOK AT TIME FIELD                           
         IC    R4,5(R2)                                                         
         MVI   1(R5),X'FF'         CHECK FOR 'ALL'                              
         CLC   8(3,R2),=C'ALL'                                                  
         BE    DT10                                                             
         LTR   R4,R4                                                            
         BZ    DT10                                                             
         GOTO1 TIMVAL,PARAS,((R4),8(R2)),1(R5)                                  
         MVI   ERROR,INVTIME                                                    
         CLI   0(R1),X'FF'                                                      
         BE    GETERR                                                           
         SPACE 1                                                                
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
         MVC   1(4,R5),DUB                                                      
         SPACE 1                                                                
DT10     ZIC   R1,NDAYTMS          INCREMENT DAY/TIME LIST COUNT                
         LA    R1,1(R1)                                                         
         STC   R1,NDAYTMS                                                       
         SPACE 1                                                                
DTEND    LA    R5,5(R5)                                                         
         BAS   RE,BUMP             NEXT DAY/DETAIL FIELD                        
         BCT   R6,DT2                                                           
         B     XIT                                                              
         EJECT                                                                  
* VALIDATE TIME EXPRESSION *                                                    
         SPACE 1                                                                
VALTIM   GOTO1 ANY                                                              
         ZIC   R0,5(R2)                                                         
         GOTO1 TIMVAL,PARAS,((R0),8(R2)),WORK                                   
         MVI   ERROR,INVTIME                                                    
         CLI   0(R1),X'FF'                                                      
         BE    GETERR                                                           
         SPACE 1                                                                
VALTIM2  LH    R1,WORK             CONVERT MILITARY TO QUARTER HOUR             
         CH    R1,=H'600'                                                       
         BNL   *+8                                                              
         LA    R1,2400(R1)                                                      
         SH    R1,=H'600'                                                       
         SR    R0,R0                                                            
         D     R0,=F'100'                                                       
         SLL   R1,2                HOURS IN R1                                  
         LR    R6,R1               1/4S IN R6                                   
         SRDA  R0,32                                                            
         D     R0,=F'15'                                                        
         AR    R6,R1                                                            
         STC   R6,ACTUAL                                                        
         B     XIT                                                              
         EJECT                                                                  
* VALIDATE DEMO PERFORMANCE LEVEL FIELD                                         
*                                                                               
* DEMMOD HAS PRIMARY DEMO MODIFIER                                              
* IF DEMMOD IS R,S, OR X, STORE INTERNALLY AS 1 DECIMAL                         
*         FOR  P,T OR Q, DEMO MUST BE INTEGER                                   
*                                                                               
* RETURNS FULL WORD DEMO VALUE IN WORK                                          
*                                                                               
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
         BH    GETERR                                                           
         SPACE 1                                                                
DLVL10   CLI   0(R3),C'.'                                                       
         BNE   DLVL40                                                           
         CLI   DEMMOD,C'R'                                                      
         BE    DLVL20                                                           
         CLI   DEMMOD,C'S'                                                      
         BE    DLVL20                                                           
         CLI   DEMMOD,C'X'                                                      
         BE    DLVL20                                                           
         MVI   ERROR,INVPERL       INVALID PERFORMANCE LEVEL                    
         B     GETERR                                                           
         SPACE 1                                                                
DLVL20   CH    R1,=H'2'            DECIMAL IS NEXT TO LAST                      
         BE    DLVL30                                                           
         MVI   ERROR,VALFMT        INVALID FORMAT                               
         B     GETERR                                                           
DLVL30   MVI   ERROR,3             MUST BE NUMERIC                              
         CLI   1(R3),C'0'                                                       
         BL    GETERR                                                           
         CLI   1(R3),C'9'                                                       
         BH    GETERR                                                           
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
         BL    GETERR                                                           
         CLI   0(R3),C'9'                                                       
         BH    GETERR                                                           
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
* VALIDATE STATION EXPRESSIONS (WTAE,WTAE/103,WABC-A)                           
*      USING STATION FILE MASTER AND MARKET RECORDS                             
         SPACE 1                                                                
VALSTA   DS    0H                                                               
         XC    ACTSTAT,ACTSTAT                                                  
         XC    ACTMKT,ACTMKT                                                    
*                                                                               
         CLI   5(R2),0                                                          
         BE    XIT                                                              
*                                                                               
         LA    R4,BLOCK                                                         
         XC    0(64,R4),0(R4)      CLEAR SCANNER TABLE                          
         GOTO1 SCANNER,DMCB,(R2),(2,(R4)),C',=/-'                               
*                                                                               
         TM    2(R4),X'80'         TEST VALID NUMERIC                           
         BO    STA20               YES - INPUT IS MARKET NUMBER                 
         CLI   0(R4),3                                                          
         BL    BADSTA                                                           
         CLI   0(R4),4                                                          
         BH    BADSTA                                                           
         TM    2(R4),X'40'         TEST ALPHA (MSPACK REQUIREMENT)              
         BZ    BADSTA                                                           
         MVC   ACTSTAT(4),12(R4)                                                
*                                                                               
         CLI   DBSELMED,C'R'       TEST RADIO                                   
         BE    STA6                                                             
         CLI   1(R4),0             TEST MEDIA ENTERED                           
         BNE   STA4                                                             
         MVI   ACTSTAT+4,C'T'                                                   
         B     STA10                                                            
*                                                                               
STA4     MVC   ACTSTAT+4(1),22(R4)                                              
         SR    R5,R5                                                            
         IC    R5,1(R4)                                                         
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   22(0,R4),=C'TV' *EXECUTED*                                       
         BE    STA10                                                            
         B     BADSTA                                                           
         SPACE 2                                                                
STA6     CLI   1(R4),2                                                          
         BH    BADSTA                                                           
         MVC   ACTSTAT+4(1),22(R4)                                              
         ZIC   R5,1(R4)                                                         
         BCTR  R5,0                                                             
         EX    R5,STAAM                                                         
         BE    STA10                                                            
         EX    R5,STAFM                                                         
         BE    STA10                                                            
         EX    R5,STACO                                                         
         BE    STA10                                                            
         B     BADSTA                                                           
STAAM    CLC   22(0,R4),=C'AM'                                                  
STAFM    CLC   22(0,R4),=C'FM'                                                  
STACO    CLC   22(0,R4),=C'CO'                                                  
         SPACE 1                                                                
* READ STATION RECORD                                                           
*                                                                               
STA10    MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'S'                                                         
         MVC   KEY+1(1),DBSELMED                                                
         CLI   DBSELMED,C'C'                                                    
         BNE   *+8                                                              
         MVI   KEY+1,C'T'                                                       
         MVC   KEY+2(5),ACTSTAT                                                 
         MVC   KEY+7(2),AGENCY                                                  
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'STATION',KEY,AIO1                     
*                                                                               
         L     R6,AIO1                                                          
         CLC   KEY(15),0(R6)       STATION FILE CONVERTED                       
         BNE   BADSTA                                                           
*                                                                               
         LA    R4,32(R4)                                                        
         CLI   0(R4),0             TEST SPILL MARKET INPUT                      
         BE    XIT                 NO                                           
         OC    4(4,R4),4(R4)                                                    
         BZ    BADSTA                                                           
         MVC   ACTMKT,6(R4)                                                     
         B     XIT                                                              
*                                                                               
BADSTA   MVI   ERROR,INVSTAT                                                    
         B     GETERR                                                           
         SPACE 2                                                                
* VALIDATE MARKET NUMBER (FOR SID) *                                            
         SPACE 1                                                                
STA20    MVC   ACTMKT,6(R4)        MOVE MARKET NUMBER                           
         GOTO1 VGETMKT                                                          
         B     XIT                                                              
         EJECT                                                                  
* READ MARKET RECORD FROM STATION FILE                                          
         SPACE 1                                                                
GETMKT   XC    KEY,KEY                                                          
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),DBSELMED                                                
         CLI   DBSELMED,C'C'                                                    
         BNE   *+8                                                              
         MVI   KEY+1,C'T'                                                       
         SR    R0,R0                                                            
         ICM   R0,3,ACTMKT                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  KEY+2(4),DUB                                                     
         MVC   KEY+6(2),AGENCY                                                  
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'STATION',KEY,AIO1                     
         L     R6,AIO1                                                          
         CLC   KEY(8),0(R6)                                                     
         BE    XIT                                                              
         MVI   ERROR,INVMKT                                                     
         B     GETERR                                                           
         EJECT                                                                  
* VALIDATE STATION EXPRESSIONS (WTAE,WTAE/103,WABC-A)                           
*  USING DEMO FILE STATIONS AND MARKETS                                         
         SPACE 1                                                                
VALDSTA  DS    0H                                                               
         XC    ACTSTAT,ACTSTAT                                                  
         XC    ACTMKT,ACTMKT                                                    
*                                                                               
         CLI   5(R2),0                                                          
         BE    XIT                                                              
*                                                                               
         LA    R4,BLOCK                                                         
         XC    0(64,R4),0(R4)      CLEAR SCANNER TABLE                          
         GOTO1 SCANNER,DMCB,(R2),(2,(R4)),C',=/-'                               
*                                                                               
         CLI   DBSELMED,C'R'                                                    
         BNE   DSTA3                                                            
         CLI   1(R4),0                                                          
         BNE   DSTA3                                                            
         CLI   0(R4),5             MUST BE STAT+BAND                            
         BNE   *+14                                                             
         MVC   ACTSTAT(5),12(R4)                                                
         B     DSTA10                                                           
*                                                                               
         CLI   0(R4),4             MUST BE STAT+BAND                            
         BNE   DSTA3                                                            
         MVC   ACTSTAT(4),12(R4)                                                
         MVC   ACTSTAT+4(1),ACTSTAT+3                                           
         MVI   ACTSTAT+3,C' '                                                   
         B     DSTA10                                                           
*                                                                               
DSTA3    TM    2(R4),X'80'         TEST VALID NUMERIC                           
         BO    DSTA20              YES - INPUT IS MARKET NUMBER                 
         CLI   0(R4),3                                                          
         BL    BADSTA                                                           
         CLI   0(R4),4                                                          
         BH    BADSTA                                                           
         TM    2(R4),X'40'         TEST ALPHA (MSPACK REQUIREMENT)              
         BZ    BADSTA                                                           
         MVC   ACTSTAT(4),12(R4)                                                
*                                                                               
         CLI   DBSELMED,C'R'       TEST RADIO                                   
         BE    DSTA6                                                            
         CLI   1(R4),0             TEST MEDIA ENTERED                           
         BNE   DSTA4                                                            
         MVI   ACTSTAT+4,C'T'                                                   
         B     DSTA10                                                           
*                                                                               
DSTA4    MVC   ACTSTAT+4(1),22(R4)                                              
         SR    R5,R5                                                            
         IC    R5,1(R4)                                                         
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   22(0,R4),=C'TV' *EXECUTED*                                       
         BE    DSTA10                                                           
         B     BADSTA                                                           
         SPACE 2                                                                
DSTA6    CLI   1(R4),2                                                          
         BH    BADSTA                                                           
         MVC   ACTSTAT+4(1),22(R4)                                              
         ZIC   R5,1(R4)                                                         
         BCTR  R5,0                                                             
         EX    R5,DSTAAM                                                        
         BE    DSTA10                                                           
         EX    R5,DSTAFM                                                        
         BE    DSTA10                                                           
         EX    R5,DSTACO                                                        
         BE    DSTA10                                                           
         B     BADSTA                                                           
DSTAAM   CLC   22(0,R4),=C'AM'                                                  
DSTAFM   CLC   22(0,R4),=C'FM'                                                  
DSTACO   CLC   22(0,R4),=C'CO'                                                  
         SPACE 1                                                                
* READ STATION RECORD                                                           
*                                                                               
DSTA10   MVC   DBAREC,AIO1                                                      
         MVI   DBFUNCT,DBVLST                                                   
         MVC   DBSELSTA,ACTSTAT                                                 
         MVC   DBFILE,=C'TP '                                                   
         GOTO1 DEMAND,DMCB,DBLOCK,0                                             
         CLI   DBERROR,X'10'                                                    
         BE    BADSTA                                                           
         SPACE 2                                                                
* GET MARKET NAME                                                               
         SPACE 1                                                                
DSTA20   MVC   ACTMKT,DBACTRMK     MOVE MARKET NUMBER                           
         B     XIT                                                              
         EJECT                                                                  
* GETERR -  SET APPROPRIATE MESSAGE AND EXIT TO SYSTEM                          
*                                                                               
* AT ENTRY  WORK =USER ERROR MESSAGE IF ERROR=SUPPLIED                          
         SPACE 1                                                                
GETERR   L     R3,ATWA             BECAUSE SOME ROUTINES GOBBLE IT              
         OI    CONHEADH+6,X'80'    ALWAYS XMT MESSAGE                           
*                                                                               
GETERR2  CLI   ERROR,SUPPLIED      TEST TEXT SUPPLIED                           
         BNE   GETERR4                                                          
         MVC   CONHEAD,WORK        USE SUPPLIED TEXT                            
         L     RD,SYSRD            AND TAKE DIRECT EXIT                         
         B     GETERRX                                                          
*                                                                               
GETERR4  CLI   ERROR,60            ERRORS BELOW 60 GO TO GENCON                 
         BH    GETERR10                                                         
         GOTO1 ERREX               AND DO NOT RETURN HERE                       
         SPACE 1                                                                
* MY OWN ERROR HANDLING - GET MESSAGE FROM TABLE *                              
         SPACE 1                                                                
GETERR10 DS    0H                                                               
         L     R1,=A(ERRTAB)                                                    
         A     R1,RELO                                                          
*                                                                               
GETERR12 CLC   0(1,R1),ERROR                                                    
         BE    GETERR14                                                         
         ZIC   RE,1(R1)            GET MESSAGE LENGTH                           
         LA    R1,2(R1,RE)         POINT TO NEXT ENTRY                          
         CLI   0(R1),X'FF'                                                      
         BNE   GETERR12                                                         
*                                                                               
GETERR14 MVC   CONHEAD(13),=C'* ERROR 999 *'                                    
         ZIC   R0,ERROR                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  CONHEAD+8(3),DUB                                                 
         ZIC   RE,1(R1)            GET MESSAGE LENGTH                           
         LA    R0,60                                                            
         CR    RE,R0                                                            
         BNH   *+6                                                              
         LR    RE,R0               MAX LENGTH 60 ON ERROR MESSAGE               
         BCTR  RE,0                SET FOR EX                                   
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   CONHEAD+14(0),2(R1)                                              
*                                                                               
GETERRX  OI    6(R2),X'40'         POSITION CURSOR TO ERROR FIELD               
         OI    CONHEADH+6,X'80'    ALWAYS TRANSMIT MESSAGE                      
         L     RD,SYSRD            AND TAKE DIRECT EXIT                         
         B     XIT                                                              
         SPACE 1                                                                
NO       LTR   RB,RB               SET CC NOT EQ                                
         B     XIT                                                              
*                                                                               
YES      CR    RB,RB                                                            
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
* THIS ROUTINE IS USED TO CALCULATE THE CURSOR POSITION WHEN AN                 
* ERROR OCCURS DURING VALIDATION USING SCANNER.                                 
* R2 MUST POINT TO FLDHDR                                                       
* R4 MUST POINT TO SCANNER ENTRY                                                
* SCAN TABLE ADDDRESS IS IN SCANADDR                                            
*  AND SCANNER ENTRY LENGTH IS IN SCANLEN                                       
         SPACE 1                                                                
CURSERR  DS    0H                  CALCULATE DSPL TO FIELD IN ERROR             
         L     R3,ATWA             BECAUSE SOME ROUTINES GOBBLE IT              
         ZIC   R0,SCANLEN                                                       
         LTR   R0,R0                                                            
         BNZ   *+8                                                              
         LA    R0,32               DEFAULT IS NORMAL                            
         L     R1,SCANADDR         GET SCANNER TABLE ADDRESS                    
         LA    R1,0(R1)            CLEAR HOB                                    
         SR    RE,RE                                                            
         SR    RF,RF                                                            
*                                                                               
CURSERR2 CR    R1,R4                                                            
         BNL   CURSERRX                                                         
         ICM   RF,1,0(R1)          GET LEN OF FIRST HALF                        
         BZ    *+8                                                              
         LA    RF,1(RF)            ADJUST FOR STOP CHAR                         
         AR    RE,RF                                                            
         ICM   RF,1,1(R1)          GET LEN OF SECOND HALF                       
         BZ    *+8                                                              
         LA    RF,1(RF)            ADJUST FOR STOP CHAR                         
         AR    RE,RF                                                            
         AR    R1,R0               NEXT SCANNER ENTRY                           
         B     CURSERR2                                                         
*                                                                               
*---------------------------------------------                                  
CURSERRX LA    RE,8(RE,R2)         POINT TO ADDRESS                             
         ST    RE,FLAST             AND SAVE IT                                 
         LA    RF,8(R2)                                                         
         SR    RE,RF                                                            
CURSULP  ZIC   RF,5(R2)            GET LENGTH OF LIST                           
         CR    RF,RE               TEST IF LENGTH IS LARGER                     
         BNL   CURSOK              YES, USE CURRENT LIST                        
         LA    R5,8(R2,RF)         ADDRESS OF LAST CHAR+1                       
         BCTR  R5,0                GET LAST CHAR                                
         LA    R2,76(R2)           NO, USE NEXT LIST                            
         CLI   0(R5),C','          SEE IF LAST CHAR WAS A ','                   
         BE    LASTCMMA            YES, DON'T CHANGE RE                         
         CLI   8(R2),C','          FIRST ONE COMMA                              
         BE    LASTCMMA            YES, DONT CHANGE RE                          
         BCTR  RE,0                SUBTRACT 1 FOR MISSING COMMA                 
LASTCMMA SR    RE,RF               USE NEW DISPLACEMENT                         
         B     CURSULP                                                          
*                                                                               
CURSOK   L     R1,ATIOB                                                         
         USING TIOBD,R1                                                         
         OI    6(R2),X'80'         XMIT ERROR FIELD HEADER                      
*                                                                               
         LR    RF,R2               COMPUTE DISPLACEMENT OF ERROR FLDH           
         S     RF,ATWA             FROM TWA START                               
         STCM  RF,3,TIOBCURD                                                    
*                                                                               
*        SR    RE,RE                                                            
*        L     RE,FLAST            PICK UP CURSOR POSITION                      
*        LA    RF,8(R2)            POINT TO DATA START                          
*        SR    RE,RF               COMPUTE DSPL INTO FIELD FOR CURSOR           
         STC   RE,TIOBCURI                                                      
         OI    TIOBINDS,TIOBSETC                                                
         B     GETERR                                                           
         EJECT                                                                  
* THIS ROUTINE IS LIKE CURSERR BUT USES THE ADDRESS THAT WAS                    
* PARSE OUT BY MINIPAR.                                                         
*  INPUT:  R2 = ADDR OF ELEMENT DATA                                            
         SPACE 1                                                                
CPRSERR  DS    0H                                                               
         L     R1,ATIOB                                                         
         USING TIOBD,R1                                                         
         USING ELEMDS,R2                                                        
         SR    RF,RF                                                            
         ICM   RF,3,ELEMADDR                                                    
         A     RF,ATWA                                                          
         OI    6(RF),X'80'         TRANSMIT ERROR FIELD HEADER                  
         MVC   TIOBCURD(2),ELEMADDR                                             
         MVC   TIOBCURI(1),ELEMDISP                                             
         OI    TIOBINDS,TIOBSETC                                                
CPRSXIT  B     GETERR                                                           
         EJECT                                                                  
* PROVIDE LINKAGE TO SPGETIUN                                                   
         SPACE 1                                                                
GOGETIUN DS    0H                                                               
         GOTO1 =V(SPGETIUN),(R1),RR=RELO                                        
         B     XIT                                                              
* CONSTANTS,TABLES,ETC                                                          
         SPACE 1                                                                
RELO     DS    A                                                                
         SPACE 1                                                                
* PATCH AREA                                                                    
*                                                                               
PATCH    DS    0H                                                               
         DC    XL32'00'                                                         
*                                                                               
BUMP     ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BR    RE                                                               
         SPACE 1                                                                
         GETEL (R6),DATADISP,ELCODE                                             
         SPACE 1                                                                
         EJECT                                                                  
*=============================================================*                 
* PROVIDE MSPACK/MSUNPK ENTRY POINTS FOR LINKAGE TO STAPACK                     
*=============================================================*                 
         SPACE 1                                                                
GOMSPACK NTR1  BASE=SYSRB,WORK=(R4,8)                                           
         L     R3,ATWA                                                          
         L     R8,ASPOOLD                                                       
         L     R9,ASYSD            RESTORE POINTER TO SYSTEM STORAGE            
         USING SYSD,R9                                                          
         L     RA,SYSRA            2ND BASE REG - R9 MUST BE SET FIRST          
         LR    R5,R1               SAVE CALLERS R1                              
*                                                                               
         USING STAPACKD,R4                                                      
         XC    0(32,R4),0(R4)                                                   
         MVI   STAPACT,C'P'                                                     
         MVC   STAPAGY,AGENCY                                                   
         MVC   STAPCTRY,CTRY                                                    
         MVC   STAPMED,DBSELMED                                                 
         CLI   STAPMED,C'C'                                                     
         BNE   *+8                                                              
         MVI   STAPMED,C'T'                                                     
         MVC   STAPACOM,ACOMFACS                                                
         L     RE,0(R5)            GET A(MKT)                                   
         MVC   STAPQMKT,0(RE)                                                   
         L     RE,4(R5)            GET A(STA)                                   
         MVC   STAPQSTA(8),0(RE)                                                
         GOTO1 STAPACK,(R4)                                                     
         CLI   STAPERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,8(R5)            GET A(MKTSTA)                                
         MVC   0(5,RE),STAPMKST                                                 
         XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
GOMSUNPK NTR1  BASE=SYSRB,WORK=(R4,8)                                           
         L     R3,ATWA                                                          
         L     R8,ASPOOLD                                                       
         L     R9,ASYSD            RESTORE POINTER TO SYSTEM STORAGE            
         USING SYSD,R9                                                          
         L     RA,SYSRA            2ND BASE REG - R9 MUST BE SET FIRST          
         LR    R5,R1               SAVE CALLERS R1                              
*                                                                               
         USING STAPACKD,R4                                                      
         XC    0(32,R4),0(R4)                                                   
         MVI   STAPACT,C'U'                                                     
         MVC   STAPAGY,AGENCY                                                   
         MVC   STAPCTRY,CTRY                                                    
         MVC   STAPMED,DBSELMED                                                 
         CLI   STAPMED,C'C'                                                     
         BNE   *+8                                                              
         MVI   STAPMED,C'T'                                                     
         MVC   STAPACOM,ACOMFACS                                                
         L     RE,0(R5)            GET A(MKTSTA)                                
         MVC   STAPMKST,0(RE)                                                   
*                                                                               
         GOTO1 STAPACK,(R4)                                                     
         CLI   STAPERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RE,4(R5)            GET A(MKT)                                   
         MVC   0(4,RE),STAPQMKT                                                 
         L     RE,8(R5)            GET A(STA)                                   
         MVC   0(5,RE),STAPQSTA    ALWAYS MOVE 5 STATION BYTES                  
         TM    0(R5),X'80'         DOES USER WANT 8 BYTES                       
         BZ    *+10                                                             
         MVC   0(8,RE),STAPQSTA                                                 
         XIT1                                                                   
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
SYSVCON  DS    0F                                                               
         DC    V(DUMMY)                                                         
NVTYPES  EQU   (*-SYSVCON)/4                                                    
         SPACE 1                                                                
ADTAB    DS    0AL3                                                             
NADCONS  EQU   (*-ADTAB)/L'ADTAB                                                
         SPACE 3                                                                
*  TABLE OF CORE RESIDENT MODULE ADDRESSES                                      
CORETAB  DS    0X                                                               
         DC    X'30'               GENCON                                       
         DC    X'14'               CLPACK                                       
         DC    X'15'               CLUNPK                                       
         DC    X'E0'               DEMOCON                                      
         DC    X'26'               DEFINE                                       
         DC    X'21'               SPGETDEM                                     
         DC    X'22'               SPDEMUP                                      
         DC    X'09'               INVEDIT                                      
         DC    X'47'               RANSID                                       
         DC    AL1(0)              GOMSPACK                                     
         DC    AL1(0)              GOMSUNPK                                     
         DC    AL1(QSTAPACK)       STAPACK                                      
CORES    EQU   (*-CORETAB)                                                      
         LTORG                                                                  
         EJECT                                                                  
*                                  AREAS                                        
CATLIST  DC    C'USA   ',AL1(0),C'     '                                        
         DC    C'MSA   ',AL1(1),C'     '                                        
         DC    C'CMA   ',AL1(1),C'     '                                        
         DC    C'TSA   ',AL1(2),C'     '                                        
         DC    C'FCA   ',AL1(2),C'     '                                        
         DC    C'ADI   ',AL1(3),C'     '                                        
         SPACE 1                                                                
*                                  CATEGORIES                                   
         DC    C'CUME  ',C'C',C' CUME'                                          
         DC    C'XCUM  ',C'E',C' XCUM'                                          
         DC    C'CUME% ',C'F',C' CUM%'                                          
         DC    C'XCUM% ',C'H',C' XCM%'                                          
         DC    C'IMPS  ',C'I',C' IMPS'                                          
         DC    C'AQH   ',C'I',C' AQH '                                          
         DC    C'CUMT  ',C'J',C' CUMT'                                          
         DC    C'AWAT  ',C'K',C' AWAT'                                          
         DC    C'AWAT% ',C'L',C' AWT%'                                          
         DC    C'AWAY  ',C'M',C' AWAY'                                          
         DC    C'AWAY% ',C'N',C' AWA%'                                          
         DC    C'PUR   ',C'P',C' PUR '                                          
         DC    C'RTG   ',C'R',C' RTG '                                          
         DC    C'CURT  ',C'T',C' CURT'                                          
         DC    C'SHARE ',C'S',C' SHR '                                          
         DC    C'SHR   ',C'S',C' SHR '                                          
         DC    C'CUSH  ',C'S',C' CUSH'                                          
         DC    C'TOTAL ',C'Q',C' TOTS'                                          
         DC    C'TSL   ',C'X',C' TSL '                                          
         DC    X'FF'                                                            
         SPACE 1                                                                
DAYTBL   DC    CL7'M-F    ',X'00'                                               
         DC    CL7'MON    ',X'01'                                               
         DC    CL7'MON-FRI',X'00'                                               
         DC    CL7'TUE    ',X'02'                                               
         DC    CL7'WED    ',X'03'                                               
         DC    CL7'THU    ',X'04'                                               
         DC    CL7'FRI    ',X'05'                                               
         DC    CL7'SAT    ',X'06'                                               
         DC    CL7'SUN    ',X'07'                                               
         DC    CL7'M-S    ',X'08'                                               
         DC    CL7'M-SU   ',X'08'                                               
         DC    CL7'MON-SUN',X'08'                                               
         DC    CL7'VAR    ',X'09'                                               
         DC    CL7'VARIOUS',X'09'                                               
         DC    X'FF'                                                            
         SPACE 1                                                                
DAYLIST  DC    CL7'M-F    ',X'00'                                               
         DC    CL7'MON    ',X'01'                                               
         DC    CL7'MON-FRI',X'00'                                               
         DC    CL7'TUE    ',X'02'                                               
         DC    CL7'WED    ',X'03'                                               
         DC    CL7'THU    ',X'04'                                               
         DC    CL7'FRI    ',X'05'                                               
         DC    CL7'SAT    ',X'06'                                               
         DC    CL7'SUN    ',X'07'                                               
         DC    CL7'M-S    ',X'08'                                               
         DC    CL7'M-SU   ',X'08'                                               
         DC    CL7'MON-SUN',X'08'                                               
         DC    CL7'S-S    ',X'09'                                               
         DC    CL7'SAT-SUN',X'09'                                               
         DC    X'FF'                                                            
         EJECT                                                                  
RECACTS  DS    0D                                                               
         SPACE 1                                                                
* X'01' ENTRIES ARE AVAILABLE REPORTS                                           
* X'04' ENTRIES SUPPORT SAVED SCREENS                                           
*                                                                               
* CL8 EXPANDED REPORT NAME                                                      
* CL1 REPORT NUMBER (01 IS RESERVED)                                            
* CL1 PHASE NUMBER FOR DATA DICTIONARY - LEAVE AS ZERO FOR NOW                  
* CL1 PHASE NUMBER FOR HELP SCREEN - LEAVE AS ZERO FOR NOW                      
         SPACE 1                                                                
         DC    X'01',C'PURE    ',AL1(02),X'0000'                                
         DC    X'01',C'FLEXI   ',AL1(03),X'0000'                                
         DC    X'04',C'RANKER  ',AL1(04),X'0000'                                
         DC    X'04',C'RESEARCH',AL1(05),X'0000'                                
         DC    X'01',C'LAYOUT  ',AL1(06),X'0000'                                
         DC    X'04',C'TREND   ',AL1(07),X'0000'                                
         DC    X'04',C'HOURLY  ',AL1(09),X'0000'                                
         DC    X'01',C'CUSTOM  ',AL1(20),X'0000'                                
         DC    X'01',C'STALIST ',AL1(21),X'0000'                                
         DC    X'01',C'MARKET  ',AL1(22),X'0000'                                
         DC    X'01',C'BOOK    ',AL1(23),X'0000'                                
         DC    X'01',C'COMPOSIT',AL1(24),X'0000'                                
         DC    X'01',C'CMBLIST ',AL1(25),X'0000'                                
         DC    X'04',C'SIMLIST ',AL1(26),X'0000'                                
         DC    X'01',C'SIDLIST ',AL1(28),X'0000'                                
         DC    X'04',C'MULTID  ',AL1(30),X'0000'                                
         SPACE 2                                                                
* X'02' ENTRIES ARE AVAILABLE ACTIONS                                           
*                                                                               
* CL8 EXPANDED ACTION NAME                                                      
* CL1 ACTION NUMBER                                                             
* CL1 ACTION EQUATE                                                             
* CL1 SPARE                                                                     
         SPACE 1                                                                
         DC    X'02',C'REPORT  ',AL1(12,12,00)                                  
         DC    X'02',C'HELP    ',AL1(00,00,00)                                  
         DC    X'02',C'ADD     ',AL1(01,01,00)                                  
         DC    X'02',C'CHANGE  ',AL1(02,01,00)                                  
         DC    X'02',C'DISPLAY ',AL1(03,01,00)                                  
         DC    X'02',C'DELETE  ',AL1(04,01,00)                                  
         DC    X'02',C'SELECT  ',AL1(05,01,00)                                  
         DC    X'02',C'RESTORE ',AL1(06,01,00)                                  
         DC    X'02',C'LIST    ',AL1(10,10,00)                                  
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
*                                 SC  SP  AC                                    
*                                   OV  RP                                      
         DC    X'03',AL1(02,12),X'D101000158',C'PUSL' PURE      REPORT          
         DC    X'03',AL1(03,12),X'D202000278',C'FLSR' FLEXI     REPORT          
         DC    X'03',AL1(04,12),X'D303000378',C'RASR' RANKER    REPORT          
         DC    X'03',AL1(05,12),X'D575009538',C'RESR' RESEARCH  REPORT          
         DC    X'03',AL1(06,12),X'D404000478',C'LASR' LAYOUT    REPORT          
         DC    X'03',AL1(07,12),X'D707000778',C'TRSR' TREND     REPORT          
         DC    X'03',AL1(09,12),X'D909000978',C'HRSR' HOURLY    REPORT          
         DC    X'03',AL1(20,01),X'E0200020F8',C'RASL' CUSTOM DP MAINT           
         DC    X'03',AL1(20,10),X'C0200020F8',C'RASL' CUSTOM DP LIST            
         DC    X'03',AL1(20,12),X'D020002058',C'RASL' CUSTOM DP REPORT          
         DC    X'03',AL1(21,01),X'E1210021F8',C'RASL' STALIST   MAINT           
         DC    X'03',AL1(21,10),X'C1210021F8',C'RASL' STALIST   LIST            
         DC    X'03',AL1(21,12),X'D621002158',C'RASL' STALIST   REPORT          
         DC    X'03',AL1(22,10),X'C222002280',C'MLSL' MARKET    LIST            
         DC    X'03',AL1(22,12),X'C222002258',C'MLSL' MARKET    REPORT          
         DC    X'03',AL1(23,10),X'C323002380',C'BLSL' BOOK      LIST            
         DC    X'03',AL1(24,12),X'C424002458',C'ACPR' COMPOSIT  REPORT          
         DC    X'03',AL1(24,10),X'C424002480',C'ACPR' COMPOSIT  LIST            
         DC    X'03',AL1(25,01),X'E5250025F8',C'RASL' CMBLIST   MAINT           
         DC    X'03',AL1(25,10),X'C5250025F8',C'RASL' CMBLIST   LIST            
         DC    X'03',AL1(25,12),X'DB25002558',C'RASL' CMBLIST   REPORT          
         DC    X'03',AL1(26,12),X'E626002678',C'SMSR' SIMLIST   REPORT          
         DC    X'03',AL1(28,12),X'D828002818',C'SDSD' SIDLIST   REPORT          
         DC    X'03',AL1(30,12),X'DA30003078',C'MUSR' MULTIDEM  REPORT          
         DC    X'FF'                                                            
         EJECT                                                                  
ERRTAB   DS    0D                                                               
*                                                                               
         DC    AL1(INVSRC,L'M61)                                                
M61      DC    C'SOURCE NOT VALID'                                              
         DC    AL1(INVBOOK,L'M62)                                               
M62      DC    C'BOOK NOT VALID'                                                
         DC    AL1(INVDEMO,L'M63)                                               
M63      DC    C'DEMO NOT VALID'                                                
         DC    AL1(INVSTAT,L'M64)                                               
M64      DC    C'STATION NOT VALID'                                             
         DC    AL1(INVMKT,L'M65)                                                
M65      DC    C'MARKET NOT VALID'                                              
         DC    AL1(INVDAYTM,L'M67)                                              
M67      DC    C'DAY/DETAIL NOT VALID'                                          
         DC    AL1(MANYBKS,L'M68)                                               
M68      DC    C' TOO MANY BOOKS - LIMIT IS 1'                                  
         DC    AL1(MANYDEM,L'M69)                                               
M69      DC    C' TOO MANY DEMOS - LIMIT IS 11'                                 
         DC    AL1(NOESTBK,L'M70)                                               
M70      DC    C' NO ESTIMATED BOOK REQUESTED'                                  
         DC    AL1(INVPERL,L'M71)                                               
M71      DC    C' INVALID DEMO PERFORMANCE LEVEL'                               
         DC    AL1(VALFMT,L'M72)                                                
M72      DC    C' FORMAT IS XX.X'                                               
         DC    AL1(TOOBIG,L'M73)                                                
M73      DC    C' SHORTEN REQUEST OR RUN OVERNIGHT'                             
         DC    AL1(INVCAT,L'M74)                                                
M74      DC    C' INVALID CATEGORY'                                             
         DC    AL1(NOTIME,L'M75)                                                
M75      DC    C' MISSING TIME ENTRY'                                           
         DC    AL1(BADSTTIM,L'M76)                                              
M76      DC    C' START TIME NOT VALID'                                         
         DC    AL1(BADNDTIM,L'M77)                                              
M77      DC    C' END TIME NOT VALID'                                           
         DC    AL1(TOOMANY,L'M78)                                               
M78      DC    C' TOO MANY ENTRIES'                                             
         DC    AL1(OVERLAP,L'M79)                                               
M79      DC    C' NO TIME OVERLAP WITHIN A GIVEN DAY'                           
         DC    AL1(OVERMAX,L'M80)                                               
M80      DC    C' RESEQ# LARGER THAN TOTAL # ELEMENTS'                          
         DC    AL1(MUSTCLR,L'M81)                                               
M81      DC    C' CLEAR EOF AFTER RESEQUENCE#'                                  
         DC    AL1(NUMER,L'M82)                                                 
M82      DC    C' RESEQUENCE# MUST BE NUMERIC'                                  
         DC    AL1(BADMENU,L'M83)                                               
M83      DC    C' INVALID DEMO MENU NAME'                                       
         DC    AL1(NOSIDPER,L'M84)                                              
M84      DC    C' NO SID PERIOD SPECIFIED'                                      
         DC    AL1(ADDNAM,L'M85)                                                
M85      DC    C' MULTIPLE DAY/TIME ENTRIES NEED NAME'                          
         DC    AL1(ONEONL,L'M86)                                                
M86      DC    C' ONLY RESEQUENCE ONE ELEMENT'                                  
         DC    AL1(DUPLI,L'M87)                                                 
M87      DC    C' DUPLICATE STATION NAMES'                                      
         DC    AL1(ONEMKT,L'M88)                                                
M88      DC    C' MUST HAVE ONE MARKET CODE'                                    
         DC    AL1(NOSTADET,L'M89)                                              
M89      DC    C' MUST HAVE STATION IN DETAILS'                                 
         DC    AL1(NOMGRSID,L'M90)                                              
M90      DC    C' MUST SPECIFY SID WITH MKTGRPS'                                
         DC    AL1(NEEDSID,L'M91)                                               
M91      DC    C' SID MUST BE SPECIFIED TO USE THIS FILTER'                     
         DC    AL1(TOOLONGA,L'M92)                                              
M92      DC    C' ALPHABETIC FILTER OPTION TOO LONG'                            
         DC    AL1(TOOLONGN,L'M93)                                              
M93      DC    C' NUMERIC FILTER OPTION TOO LONG'                               
         DC    AL1(INVINPT,L'M94)                                               
M94      DC    C' MUST BE ENTIRELY ALPHA OR NUMERIC'                            
         DC    AL1(TABFUL,L'M95)                                                
M95      DC    C' TABLE IS FULL. SHORTEN REQUEST'                               
         DC    AL1(INVOPT,L'M96)                                                
M96      DC    C' INVALID REPORT OPTION'                                        
         DC    AL1(INVDPT,L'M97)                                                
M97      DC    C' INVALID DAYPART FIELD'                                        
         DC    AL1(NOTIMP,L'M98)                                                
M98      DC    C' DEMOS REQUESTED MUST BE IMPS'                                 
         DC    AL1(ONLTOT,L'M99)                                                
M99      DC    C' OPTION MUST BE TOT ON LIST'                                   
         DC    AL1(NOSCHYET,L'M100)                                             
M100     DC    C' MUST ENTER SCHEME BEFORE PERIOD'                              
         DC    AL1(BADCATBK,L'M101)                                             
M101     DC    C' TSA/ADI INVALID FOR BOOK'                                     
         DC    AL1(255,L'UNKNOWN)                                               
UNKNOWN  DC    C'MESSAGE NOT DEFINED'                                           
*                                                                               
XXXX     DS    0H                                                               
*------------------------------------------------------------------             
*      BTYPE,MRKT NO.,START BOOK,END BOOK                                       
E1BTAB   DC    C'B',AL2(934),AL1(00,00),AL1(255,255)  BOSTON                    
         DC    C'B',AL2(935),AL1(00,00),AL1(255,255)  BUFFALO                   
         DC    C'B',AL2(936),AL1(00,00),AL1(255,255)  CHATTANOOGA               
         DC    C'B',AL2(937),AL1(00,00),AL1(255,255)  GRANV/SPARTA              
         DC    C'B',AL2(946),AL1(00,00),AL1(255,255)  HUNTSVILLE                
         DC    C'B',AL2(947),AL1(00,00),AL1(255,255)  LITTLE ROCK               
         DC    C'B',AL2(939),AL1(00,00),AL1(255,255)  MILWAUKEE                 
         DC    C'B',AL2(949),AL1(00,00),AL1(255,255)  NASSAU/SUFFOLK            
         DC    C'B',AL2(940),AL1(00,00),AL1(255,255)  ORLANDO                   
         DC    C'B',AL2(941),AL1(00,00),AL1(255,255)  PITTSBURG                 
         DC    C'B',AL2(950),AL1(00,00),AL1(255,255)  ROANOKE                   
         DC    C'B',AL2(942),AL1(00,00),AL1(255,255)  TAMPA                     
         DC    C'B',AL2(943),AL1(00,00),AL1(255,255)  TOLEDO                    
         DC    C'H',AL2(782),AL1(00,00),AL1(255,255)  ANAHIEM                   
         DC    C'H',AL2(783),AL1(00,00),AL1(095,011)  BAKERSFIELD               
         DC    C'H',AL2(846),AL1(00,00),AL1(095,011)  DALLAS                    
         DC    C'H',AL2(784),AL1(00,00),AL1(095,011)  DENVER                    
         DC    C'H',AL2(861),AL1(00,00),AL1(095,011)  FRESNO                    
         DC    C'H',AL2(862),AL1(00,00),AL1(095,011)  MONT/SALINAS              
         DC    C'H',AL2(864),AL1(00,00),AL1(095,011)  PHOENIX                   
         DC    C'H',AL2(865),AL1(00,00),AL1(095,011)  TUCSON                    
         DC    X'FF'                                                            
*------------------------------------------------------------------             
       ++INCLUDE SPGENAGY                                                       
         EJECT                                                                  
       ++INCLUDE FATIOB                                                         
         SPACE 2                                                                
* DDCOMFACS - COMMON FACILITIES DSECT                                           
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
*                                                                               
* FAFACTS - SYSTEM INFO BLOCK                                                   
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
         SPACE 1                                                                
*                                                                               
* CTGENFILE - CONTROL FILE RECORD DSECTS                                        
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
         EJECT                                                                  
BADCATBK EQU   101                                                              
       ++INCLUDE SPRESWORKD                                                     
         PRINT OFF                                                              
       ++INCLUDE SPSTAPACKD                                                     
         EJECT                                                                  
* DDCOREQUS                                                                     
       ++INCLUDE DDCOREQUS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
ELEMDS   DSECT                                                                  
ELEMSTOP DS    C                                                                
ELEMLEN  DS    X                                                                
ELEMADDR DS    XL2                                                              
ELEMDISP DS    X                                                                
ELEMDATA DS    0C                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'085SPRES00   03/02/09'                                      
         END                                                                    
