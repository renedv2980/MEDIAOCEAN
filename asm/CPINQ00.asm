*          DATA SET CPINQ00    AT LEVEL 006 AS OF 05/01/02                      
*PHASE TC0300A                                                                  
*INCLUDE BINSRCH                                                                
         TITLE 'COST PER POINT INQUIRY PROGRAM - ROOT PHASE'                    
TC0300   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 750,**CINQ**,RR=R5                                               
         ST    R5,RELO                                                          
         B     ST                                                               
RELO     DC    A(0)                                                             
ST       LA    R8,2048(RB)                                                      
         LA    R8,2048(R8)                                                      
         USING TC0300+4096,R8      R8 = 2ND BASE                                
         USING GWS,RC              RC = GLOBAL W/S                              
         L     RA,4(R1)                                                         
         USING TWAD,RA                                                          
         MVC   TERMINAL,TWATRM                                                  
         MVC   WORK(2),TWAAGY                                                   
         USING TC03TWA,RA          RA = TWA                                     
         MVC   AGENCY,WORK                                                      
         SPACE 1                                                                
         ST    RB,ABASE            STORE SOME USEFUL ADDRESSES                  
         ST    R8,A2NDBASE                                                      
         ST    RD,AREGSAVE                                                      
         LA    R2,IO                                                            
         ST    R2,AKEY                                                          
         MVC   ATIA,12(R1)                                                      
         L     RE,16(R1)                                                        
         USING COMFACSD,RE                                                      
         MVC   VDATAMGR,CDATAMGR                                                
         MVC   VCALLOV,CCALLOV                                                  
         MVC   VGETMSG,CGETMSG                                                  
         MVC   VSCANNER,CSCANNER                                                
         MVC   VDATVAL,CDATVAL                                                  
         MVC   VDATCON,CDATCON                                                  
         L     RE,=V(BINSRCH)                                                   
         A     RE,RELO                                                          
         ST    RE,VBINSRCH                                                      
         SPACE 1                                                                
         LA    RE,GWS              ZEROISE FIRST PART OF GLOBAL W/S             
         LA    RF,INITEND                                                       
         SR    RF,RE                                                            
         XCEF                                                                   
         SPACE 1                                                                
         LA    R2,ACOMMON          RELOCATE COMMON S/R ADDRESSES AND            
         LA    R3,COMMON           STORE IN GLOBAL                              
         LA    R4,COMMONX-COMMON                                                
         SRL   R4,2                                                             
T01      L     R5,0(R2)                                                         
         A     R5,RELO                                                          
         ST    R5,0(R3)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,T01                                                           
         SPACE 1                                                                
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
         MVC   CPQHEAD,SPACES                                                   
         OI    CPQHEADH+6,X'80'                                                 
         SPACE 1                                                                
         MVC   HEADING1,CPQDAT1    KEEP HEADLINES                               
         MVC   HEADING2,CPQDAT2                                                 
         LA    R2,CPQDAT1H         CLEAR MAX HEADLINES                          
         LA    R7,CPQDAT3H                                                      
         LA    R6,86                                                            
         OI    6(R2),X'80'                                                      
         MVC   8(L'CPQDAT1,R2),SPACES                                           
         BXLE  R2,R6,*-10                                                       
         LA    R7,CPQDATLH         AND USED DATA LINES                          
T02      OC    8(L'CPQDAT1,R2),8(R2)                                            
         BE    T03                                                              
         OI    6(R2),X'80'                                                      
         XC    8(L'CPQDAT1,R2),8(R2)                                            
         BXLE  R2,R6,T02                                                        
T03      DS    0H                                                               
         EJECT                                                                  
*                  VALIDATE AND SAVE CLIENT INPUT                               
         SPACE 3                                                                
T04      LA    R2,CPQCLIH                                                       
         MVI   FERN,MISSING                                                     
         CLI   5(R2),0                                                          
         BE    ERROR                                                            
         CLC   CPQCLI,SAVECNAM     ALREADY VALIDATED                            
         BE    T11                                                              
         XC    SAVECNAM,SAVECNAM                                                
         XC    SAVEKEY(9),SAVEKEY                                               
         MVI   SAVEKEY,X'06'       BANK                                         
         MVI   SAVEKEY+3,C'T'                                                   
         CLC   CPQCLI(4),=C'BANK'                                               
         BNE   T05                                                              
         XC    CPQCLI+4(L'CPQCLI-4),CPQCLI+4                                    
         B     T10                                                              
         SPACE 3                                                                
T05      MVI   SAVEKEY,X'04'       ALL                                          
         MVC   SAVEKEY+1(2),AGENCY                                              
         CLC   CPQCLI(3),=C'ALL'                                                
         BNE   T06                                                              
         XC    CPQCLI+3(L'CPQCLI-3),CPQCLI+3                                    
         B     T10                                                              
         SPACE 3                                                                
T06      MVI   SAVEKEY,X'02'       CLIENT                                       
         MVI   FERN,CLINTNFL                                                    
         MVC   SAVEKEY+6(3),CPQCLI                                              
         OC    SAVEKEY+6(3),SPACES                                              
         CLI   5(R2),4                                                          
         BL    T07                                                              
         CLI   5(R2),5                                                          
         BNE   ERROR                                                            
         MVC   FULL,=4C'0'                                                      
         MVZ   FULL,CPQCLI+1                                                    
         CLC   FULL,=4C'0'                                                      
         BNE   ERROR                                                            
         PACK  DUB,CPQCLI+1(4)                                                  
         CVB   R4,DUB                                                           
         STH   R4,HALF                                                          
         MVC   SAVEKEY+7(2),HALF                                                
         SPACE 1                                                                
T07      MVC   KEY(9),SAVEKEY      READ A CLIENT RECORD                         
         MVI   KEY,X'12'                                                        
         BAS   RE,READ                                                          
         LA    R9,IO                                                            
         USING CPKEYD,R9                                                        
         LA    R9,CPRECORD                                                      
         SR    R3,R3                                                            
T08      CLI   0(R9),0                                                          
         BNE   *+14                                                             
         MVC   CPQCLI+5(16),=C'NAME NOT ON FILE'                                
         B     T10                                                              
         CLI   0(R9),X'04'         NAME ELEMENT                                 
         BE    T09                                                              
         IC    R3,1(R9)                                                         
         AR    R9,R3                                                            
         B     T08                                                              
         SPACE 1                                                                
T09      DS    0H                  MOVE NAME TO SCREEN FOR UP TO 24 CHS         
         USING CPNAMED,R9                                                       
         IC    R3,CPNLEN                                                        
         SH    R3,=H'3'                                                         
         LA    R4,23                                                            
         CR    R3,R4                                                            
         BNL   *+6                                                              
         LR    R4,R3                                                            
         XC    CPQCLI,CPQCLI                                                    
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   CPQCLI(0),CPNAME                                                 
         SPACE 3                                                                
T10      OI    6(R2),X'80'         SET TRANSMIT BIT                             
         MVC   SAVECNAM,CPQCLI                                                  
         MVI   NEXT,0              CLEAR CONTINUATION SCREEN MARKER             
         B     T11                                                              
         EJECT                                                                  
*                  VALIDATE AND SAVE DATES INPUT                                
         SPACE 3                                                                
T11      LA    R2,CPQDATH                                                       
         TM    4(R2),X'20'         ALREADY VALIDATED                            
         BO    T15                                                              
         MVI   FERN,MISSING                                                     
         CLI   5(R2),0                                                          
         BE    ERROR                                                            
         XC    STARTDAT(6),STARTDAT CLEAR START/END DATES                       
         SPACE 1                                                                
         MVI   FERN,DATINVAL                                                    
         GOTO1 VDATVAL,DMCB,(2,CPQDAT),WORK                                     
         CLI   DMCB+3,0                                                         
         BE    ERROR                                                            
         LA    R3,CPQDAT+4         LOOK FOR SEPARATOR                           
         LA    R4,1                                                             
         LA    R5,CPQDAT+6                                                      
         CLI   0(R3),C'-'                                                       
         BE    *+12                                                             
         BXLE  R3,R4,*-8                                                        
         B     T12                                                              
         GOTO1 VDATVAL,DMCB,(2,1(R3)),WORK+6                                    
         CLI   DMCB+3,0                                                         
         BNE   T13                                                              
T12      MVI   FNDX,2                                                           
         B     ERROR                                                            
         SPACE 1                                                                
T13      GOTO1 VDATCON,DMCB,(0,WORK),(3,STARTDAT)                               
         GOTO1 VDATCON,DMCB,(0,WORK+6),(3,ENDDAT)                               
         ZIC   R3,STARTDAT                                                      
         CLI   STARTDAT,50         IF YR<51 ADD 100                             
         BH    *+8                                                              
         AH    R3,=H'100'                                                       
         STC   R3,STARTDAT                                                      
         MH    R3,=H'12'           CONVERT TO MONTHS FOR RANGE CHECKS           
         ZIC   R4,STARTDAT+1                                                    
         BCTR  R4,0                                                             
         AR    R3,R4                                                            
         STH   R3,STAMNTHS                                                      
         IC    R4,ENDDAT                                                        
         CLI   ENDDAT,50                                                        
         BH    *+8                                                              
         AH    R4,=H'100'                                                       
         STC   R4,ENDDAT                                                        
         MH    R4,=H'12'                                                        
         ZIC   R5,ENDDAT+1                                                      
         BCTR  R5,0                                                             
         AR    R4,R5                                                            
         STH   R4,ENDMNTHS                                                      
         CR    R3,R4                                                            
         MVI   FERN,STLATEND       ERROR IF START > END                         
         BH    ERROR                                                            
         SH    R4,=H'12'                                                        
         MVI   RANGEMK,1           RANGEMK = 1 FOR NOT >1YR                     
         CR    R3,R4                                                            
         BH    T14                                                              
         MVI   RANGEMK,3                   = 3 FOR >1YR & NOT >3YRS             
         SH    R4,=H'24'                                                        
         CR    R3,R4                                                            
         BH    T14                                                              
         MVI   RANGEMK,6                   = 6 FOR >3YRS & NOT >6YRS            
         SH    R4,=H'36'                                                        
         CR    R3,R4                                                            
         BH    T14                                                              
         MVI   RANGEMK,12                  =12 FOR >6YRS & NOT >12YRS           
         SH    R4,=H'72'                                                        
         CR    R3,R4                                                            
         BH    T14                                                              
         MVI   FERN,DTRNGNVL       RANGE OF >12YRS INVALID                      
         B     ERROR                                                            
         SPACE 1                                                                
T14      MVC   STARTDAT+2(1),STARTDAT+1 ARRANGE BOTH DATES AS IN CPP            
         MVI   STARTDAT+1,X'0B'         FILE PERFORMANCE ELEMENT                
         MVC   ENDDAT+2(1),ENDDAT+1                                             
         MVI   ENDDAT+1,X'0B'                                                   
         SPACE 1                                                                
         OI    4(R2),X'20'         SET VALIDATED BIT                            
         MVI   NEXT,0              CLEAR CONTINUATION SCREEN MARKER             
         B     T15                                                              
         EJECT                                                                  
*                  VALIDATE AND SAVE REPORT AND DATA TYPES INPUT                
         SPACE 3                                                                
T15      LA    R2,CPQTYPH                                                       
         CLC   CPQTYP,SAVETYPE     ALREADY VALIDATED                            
         BE    *+8                                                              
         MVI   NEXT,0                                                           
         MVI   REPTYPE,3           DEFAULT IS GUIDE/CPP                         
         MVI   DATATYPE,1                                                       
         MVC   AMATRXSR,ACPPUPD                                                 
         MVC   AFORMTSR,ACPPDISP                                                
         MVC   SAVETYPE,=C'GUIDE-CPP'                                           
         CLI   5(R2),0                                                          
         BE    T20                                                              
         SPACE 1                                                                
         XC    SCANBLK(32),SCANBLK                                              
         GOTO1 VSCANNER,DMCB,CPQTYPH,(1,SCANBLK),C',=,-'                        
         MVI   FERN,INVALID                                                     
         CLI   DMCB+4,0                                                         
         BE    ERROR                                                            
         ZIC   R3,SCANBLK          LENGTH OF REPORT TYPE INPUT                  
         BCTR  R3,0                                                             
         LTR   R3,R3                                                            
         BNL   *+10                                                             
         SR    R3,R3                                                            
         MVI   SCANBLK+12,C'G'     SET TO G(UIDE) IF MISSING                    
         LA    R4,REPTAB                                                        
         USING REPTABD,R4                                                       
         SPACE 1                                                                
T16      CLI   0(R4),X'FF'         LOOK FOR REPORT TYPE IN TABLE                
         BE    ERROR                                                            
         EX    R3,T16COMP                                                       
         BE    T17                                                              
         LA    R4,L'REPTAB(R4)                                                  
         B     T16                                                              
T16COMP  CLC   REPNAME(0),SCANBLK+12                                            
         SPACE 1                                                                
T17      MVC   REPTYPE,REPNUMB     SAVE REPORT NUMBER (=OVERLAY NO)             
         MVC   SAVETYPE(6),REPNAME                                              
         XC    SAVETYPE+6(3),SAVETYPE+6                                         
         MVC   WORK(1),REPMASK     WORK=DATA TYPE COMPATIBILITY BITS            
         SPACE 1                                                                
         ZIC   R3,SCANBLK+1        LENGTH OF DATA TYPE INPUT                    
         BCTR  R3,0                                                             
         LTR   R3,R3                                                            
         BNL   *+14                                                             
         LA    R3,2                SET TO CPP IF MISSING                        
         MVC   SCANBLK+22(3),=C'CPP'                                            
         LA    R4,DATATAB                                                       
         USING DATATABD,R4                                                      
         SPACE 1                                                                
T18      CLI   0(R4),X'FF'         LOOK FOR DATA TYPE IN TABLE                  
         BNE   *+12                                                             
         MVI   FNDX,2                                                           
         B     ERROR                                                            
         EX    R3,T18COMP                                                       
         BE    T19                                                              
         LA    R4,L'DATATAB(R4)                                                 
         B     T18                                                              
T18COMP  CLC   DATNAME(0),SCANBLK+22                                            
         SPACE 1                                                                
T19      CLI   REPTYPE,5           IF NOT DETAIL                                
         BE    T20                                                              
         ZIC   R5,DATNUMB          CHECK REPORT/DATA TYPE COMPATIBILITY         
         EX    R5,*+8                                                           
         B     *+8                                                              
         TM    WORK,0                                                           
         MVI   FERN,REPDATNC                                                    
         BNO   ERROR                                                            
         STC   R5,DATATYPE         SAVE DATA TYPE AND ADDRESSES OF              
         ZIC   R5,DATUPDSR,2       UPDATE AND DISPLAY S/R'S FOR THIS            
         AR    R5,RC               DATA TYPE                                    
         MVC   AMATRXSR,0(R5)                                                   
         ZIC   R5,DATDSPSR,2                                                    
         AR    R5,RC                                                            
         MVC   AFORMTSR,0(R5)                                                   
         MVI   SAVETYPE+5,C'-'                                                  
         MVC   SAVETYPE+6(3),DATNAME                                            
         SPACE 1                                                                
T20      MVC   CPQTYP,SAVETYPE                                                  
         OI    6(R2),X'80'                                                      
         EJECT                                                                  
*                  VALIDATE AND SAVE MARKET INPUT                               
         SPACE 3                                                                
T22      LA    R2,CPQMARH                                                       
         MVI   FERN,MISSING                                                     
         CLI   5(R2),0                                                          
         BE    ERROR                                                            
         OC    CPQMAR,SPACES                                                    
         CLC   CPQMAR,SAVEMNAM                                                  
         BE    T27                                                              
         XC    SAVEMNAM,SAVEMNAM                                                
         XC    SAVEMARK,SAVEMARK                                                
         SPACE 1                                                                
*&&UK                                                                           
         MVI   FERN,MKNAMNVL       IF UK MUST BE ALPHA                          
         TM    4(R2),X'08'                                                      
         BO    ERROR                                                            
         ZIC   R5,5(R2)                                                         
         BCTR  R5,0                                                             
         LA    R9,UKSTATS                                                       
         USING UKSTATSD,R9                                                      
         SPACE 1                                                                
T25      OC    UKCODE,UKCODE       FIND CODE FIND CODE FROM LOOK-UP TABL        
         BZ    ERROR                                                            
         EX    R5,T25COMP                                                       
         BE    T26                                                              
         LA    R9,L'UKSTATS(R9)                                                 
         B     T25                                                              
T25COMP  CLC   CPQMAR(0),UKNAME                                                 
         SPACE 1                                                                
T26      MVC   SAVEMARK,UKCODE                                                  
         CLC   UKNAME,CPQMAR                                                    
         BE    *+14                                                             
         MVC   CPQMAR(L'UKNAME),UKNAME                                          
*&&                                                                             
*&&US                                                                           
         GOTO1 VCALLOV,DMCB,(1,0),(0,TC03TWA)                                   
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     R9,DMCB             R9=A(CPP MARKET TABLE) =OVERLAY 1            
         USING CPMKTSD,R9                                                       
         SPACE 1                                                                
         SPACE 1                                                                
         MVC   WORK(3),=3C'0'      IF 1ST 3 CHARACTERS ARE NUMERIC              
         MVZ   WORK(3),CPQMAR                                                   
         CLC   WORK(3),=3C'0'                                                   
         BNE   T24                                                              
         MVI   FERN,NSINVAL                                                     
         PACK  DUB,CPQMAR(3)                                                    
         CVB   R4,DUB                                                           
         SPACE 1                                                                
T23      CLI   0(R9),X'FF'         SEARCH FOR MATCHING NSI CODE                 
         BE    ERROR                                                            
         CH    R4,CPMKNSI                                                       
         BE    T26                                                              
         LA    R9,L'CPMKLEN(R9)                                                 
         B     T23                                                              
         SPACE 1                                                                
T24      MVI   FERN,MKNAMNVL       IF INPUT NOT NUMERIC                         
         ZIC   R5,5(R2)                                                         
         BCTR  R5,0                                                             
         SPACE 1                                                                
T25      CLI   0(R9),X'FF'         SEARCH FOR MATCHING MARKET NAME              
         BE    ERROR                                                            
         EX    R5,T25COMP                                                       
         BE    T26                                                              
         BL    ERROR                                                            
         LA    R9,L'CPMKLEN(R9)                                                 
         B     T25                                                              
T25COMP  CLC   CPQMAR(0),CPMKNAM                                                
         SPACE 3                                                                
T26      MVC   SAVEMARK,CPMKNSI    IF MATCHING ENTRY FOUND,SAVE NSI             
         MVC   CPQMAR,CPMKNAM      AND DISPLAY FULL MARKET NAME                 
*&&                                                                             
         SPACE 1                                                                
         OI    6(R2),X'80'                                                      
         MVC   SAVEMNAM,CPQMAR                                                  
         MVI   NEXT,0              CLEAR CONTINUATION SCREEN MARKER             
         SPACE 3                                                                
T27      MVC   SAVEKEY+4(2),SAVEMARK     MOVE NSI CODE INTO SAVED KEY           
         B     T28                                                              
         EJECT                                                                  
*                  VALIDATE AND SAVE DEMOS INPUT                                
         SPACE 3                                                                
T28      LA    R2,CPQDEMH                                                       
         MVI   FERN,MISSING                                                     
         CLI   5(R2),0                                                          
         BE    ERROR                                                            
         OC    CPQDEM,SPACES                                                    
         CLC   CPQDEM,SAVEDNAM                                                  
         BE    T43                                                              
         MVC   SAVEDNAM,SPACES                                                  
         XC    SAVEKEY+9(2),SAVEKEY+9                                           
         MVI   SAVEMENU,0                                                       
         SPACE 3                                                                
         XC    SCANBLK(32),SCANBLK                                              
         GOTO1 VSCANNER,DMCB,CPQDEMH,(1,SCANBLK),C',=,/'                        
         MVI   FERN,DEMINVAL                                                    
         CLI   DMCB+4,0                                                         
         BE    ERROR                                                            
         GOTO1 VCALLOV,DMCB,(2,0),(0,TC03TWA)                                   
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     R9,DMCB             R9=A(DEMO NAME TABLE) = OVERLAY 2            
         SPACE 1                                                                
         SR    R4,R4               R4 = DEMO NUMBER                             
         OC    SCANBLK+12(7),SPACES                                             
         SPACE 1                                                                
T32      CLI   0(R9),X'FF'                                                      
         BE    ERROR                                                            
         LA    R4,1(R4)                                                         
         TM    SCANBLK+2,X'80'     NUMERIC                                      
         BNO   T33                                                              
         C     R4,SCANBLK+4                                                     
         BE    T34                                                              
         B     *+14                                                             
T33      CLC   0(7,R9),SCANBLK+12                                               
         BE    T34                                                              
         LA    R9,7(R9)                                                         
         B     T32                                                              
         SPACE 1                                                                
T34      CLI   0(R9),C'R'                                                       
         BE    ERROR                                                            
         CLC   0(7,R9),=C'UNKNOWN'                                              
         BE    ERROR                                                            
         MVC   SAVEDNAM(7),0(R9)                                                
         CH    R4,=H'128'          IS IT A MENU                                 
         BNH   T36                                                              
         SH    R4,=H'128'                                                       
         STC   R4,SAVEMENU         SAVE MENU NUMBER                             
         MVI   FERN,MENUNVAL                                                    
         LA    R3,MENULIST                                                      
         SPACE 1                                                                
T35      CLI   0(R3),X'FF'         CHECK THAT WE COVER THIS MENU                
         BE    ERROR                                                            
         CLC   SAVEMENU,0(R3)                                                   
         BE    T41                                                              
         LA    R3,1(R3)                                                         
         B     T35                                                              
         SPACE 3                                                                
T36      STC   R4,SAVEKEY+9        SAVE NUMBER = POSITION IN TABLE              
         MVC   SAVEKEY+10(1),SAVEKEY+9  EQUATE SUB-DEMO NO IN SAVEKEY           
         SPACE 3                                                                
         CLI   SCANBLK+1,0         ANY SUB-DEMO INPUT - HANDLE AS               
         BE    T41                 TARGET-DEMO INPUT                            
         SR    R4,R4                                                            
         OC    SCANBLK+22(7),SPACES                                             
         L     R9,DMCB                                                          
         SPACE 1                                                                
T37      CLI   0(R9),X'FF'                                                      
         BE    ERROR                                                            
         LA    R4,1(R4)                                                         
         TM    SCANBLK+3,X'80'                                                  
         BNO   T38                                                              
         C     R4,SCANBLK+8                                                     
         BE    T39                                                              
         B     *+14                                                             
T38      CLC   0(7,R9),SCANBLK+22                                               
         BE    T39                                                              
         LA    R9,7(R9)                                                         
         B     T37                                                              
         SPACE 1                                                                
T39      STC   R4,SAVEKEY+10                                                    
         CLC   SAVEKEY+9(1),SAVEKEY+10                                          
         BE    T41                                                              
         CLI   0(R9),C'R'                                                       
         BE    T42                                                              
         CLC   0(7,R9),=C'UNKNOWN'                                              
         BE    ERROR                                                            
         MVI   SAVEDNAM+7,C'/'                                                  
         MVC   SAVEDNAM+8(7),0(R9)                                              
         SPACE 3                                                                
T41      MVC   CPQDEM,SAVEDNAM                                                  
         OI    6(R2),X'80'                                                      
         MVI   NEXT,0              CLEAR CONTINUATION SCREEN MARKER             
         B     T43                                                              
         SPACE 1                                                                
T42      MVI   FNDX,2              SET FNDX FOR ERRORS IN 2ND HALF OF           
         B     ERROR               INPUT                                        
         SPACE 3                                                                
T43      CLI   SAVEMENU,0          REPORT TYPE MUST BE GUIDE IF MENU            
         BE    T43A                                                             
         MVI   FERN,MENUNTGD                                                    
         CLI   REPTYPE,3                                                        
         BNE   ERROR                                                            
T43A     DS    0H                                                               
         EJECT                                                                  
*                  VALIDATE AND SAVE FILTERS INPUT                              
         SPACE 3                                                                
         LA    R2,CPQFILH                                                       
         LA    R6,FILTAB                                                        
         USING FTABD,R6                                                         
         CLI   5(R2),0             NO FILTERS                                   
         BE    T52                                                              
         XC    SCANBLK(128),SCANBLK                                             
         GOTO1 VSCANNER,DMCB,CPQFILH,(4,SCANBLK),C',=,='                        
         CLI   DMCB+4,0                                                         
         MVI   FERN,INVALID                                                     
         BE    ERROR                                                            
         LA    R3,SCANBLK                                                       
         SPACE 2                                                                
T44      CLI   0(R3),0             LOOP TO CHECK A FILTER                       
         BE    T52                                                              
         CLI   0(R3),2             KEYWORD MUST BE 2 CHARS                      
         BH    ERROR                                                            
         LA    R4,FILTERS                                                       
         USING FILTD,R4                                                         
         ZIC   R5,0(R3)            R5 = L OF LHS OF FILTER                      
         BCTR  R5,0                                                             
         SPACE 1                                                                
T45      CLI   FILTKEY,X'FF'       CHECK KEYWORD AGAINST TABLE OF               
         BE    ERROR               AVAILABLE FILTERS                            
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   FILTKEY(0),12(R3)                                                
         BE    T46                                                              
         LA    R4,L'FILTERS(R4)                                                 
         B     T45                                                              
         SPACE 1                                                                
T46      ZIC   R7,1(R3)            COME HERE WITH A MATCHING TAB ENTRY          
         BCTR  R7,0                                                             
         LTR   R7,R7               R7=L-1 OF RHS OF FILTER                      
         BM    ERROR                                                            
         MVC   WORK(1),1(R3)       WORK=L OF RHS OF FILTER                      
         LA    R1,22(R3)           R1  =POINTER TO RHS OF FILTER                
         MVI   FTABSIGN,C'P'       SET POSITIVE OR NEGATIVE MARKER              
         CLI   0(R1),C'*'                                                       
         BNE   T47                                                              
         STC   R7,WORK             ADJUST R1,R7,WORK IF NEGATIVE                
         BCTR  R7,0                                                             
         LTR   R7,R7                                                            
         BM    ERROR                                                            
         LA    R1,1(R1)                                                         
         MVI   FTABSIGN,C'N'                                                    
         SPACE                                                                  
T47      CLI   FILTNTNL,C' '       FILTER HAS A NOTIONAL VALUE                  
         BE    T49                                                              
         CH    R7,=H'4'                                                         
         BNH   *+8                                                              
         LA    R7,4                                                             
         EX    R7,*+8                                                           
         B     *+10                                                             
         CLC   FILTNTNL(0),0(R1)   DOES IT MATCH INPUT                          
         BE    T48                                                              
T47A     LA    R4,L'FILTERS(R4)    IF NOT THIS TAB ENTRY DOESN'T FIT            
         B     T45                                                              
         SPACE 1                                                                
T48      MVI   FTABMARK,C'M'       IF MATCH, SET 'MASK' MARKER AND GET          
         MVC   FTABVAL(4),FILTVAL  REAL VALUE                                   
         MVC   FTABLEN,FILTLEN     AND LENGTH FROM TAB ENTRY                    
         B     T50                                                              
         SPACE 1                                                                
T49      CLC   FILTLEN,WORK        FILTER DOESN'T HAVE NOTIONAL VALUE           
         BL    T47A                IF ITS TOO LONG TAB ENTRY DOESNT FIT         
         MVI   FTABMARK,C'C'       SET 'COMPARE' MARKER                         
         EX    R7,*+8                                                           
         B     *+10                                                             
         MVC   FTABVAL(0),0(R1)    GET REAL VALUE                               
         MVC   FTABLEN,WORK        AND LENGTH FROM INPUT                        
         L     R5,FILTVAL          IF THERE IS A CONVERSION S/R                 
         LTR   R5,R5               RELOCATE IT                                  
         BZ    *+6                                                              
         A     R5,RELO                                                          
         ST    R5,FTABSR                                                        
         SPACE 1                                                                
T50      MVC   HALF,FILTELMT       GET POINTER TO ELEMENT/DISPLACEMENT          
         LH    R5,HALF             WITHIN ELEMENT FROM TAB ENTRY                
         AR    R5,RC                                                            
         ST    R5,FTABELMT                                                      
         MVC   FTABDISP,FILTDISP                                                
         SPACE 2                                                                
T51      LA    R3,L'SCANBLK(R3)    BUMP TO NEXT FILTER INPUT                    
         ZIC   RF,FNDX             INCREMENT FNDX (POINTER TO SUBFIELD          
         LTR   RF,RF                                                            
         BNZ   *+8                                                              
         LA    RF,1                                                             
         LA    RF,1(RF)            FOR ERROR REPORTING)                         
         STC   RF,FNDX                                                          
         LA    R6,L'FILTAB(R6)                                                  
         B     T44                                                              
         SPACE 3                                                                
T52      MVI   FTABELMT,X'FF'      PUT TERMINATOR ON GENERATED FILTAB           
         MVI   FNDX,0                                                           
         B     T53                                                              
         EJECT                                                                  
*                  CALL OVERLAY FOR THIS REPORT TYPE AND PASS CONTROL           
*                  TO OVERLAY TO GENERATE A MATRIX AND SET UP DISPLAY           
*                  SCREEN FROM IT                                               
         SPACE 3                                                                
T53      CLI   NEXT,0                                                           
         BNE   T54                                                              
         XC    MAINTAB(240),MAINTAB                                             
         MVI   NOHEDLNS,1                                                       
         MVC   HEADING1,SPACES                                                  
         MVC   HEADING2,SPACES                                                  
         LA    RE,MATRIX                                                        
         LA    RF,L'MATRIX                                                      
         MH    RF,MAXRECS+2                                                     
         XCEF                                                                   
         SPACE 1                                                                
T54      LA    R2,CPQCLIH                                                       
         MVI   FERN,X'FF'                                                       
         GOTO1 VCALLOV,DMCB,(REPTYPE,0),(0,TC03TWA)                             
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   APHASE,0(R1)                                                     
         L     RF,APHASE                                                        
         GOTO1 (RF),DMCB,(RC)                                                   
         SPACE 1                                                                
         CLI   FERN,X'FF'                                                       
         BNE   ERROR                                                            
         MVC   CPQHEAD(40),=C'ENQUIRY COMPLETE - ENTER FIELDS FOR NEXT'         
         CLI   NEXT,0                                                           
         BE    EXIT                                                             
         MVC   CPQHEAD(40),=CL40'HIT ENTER FOR CONTINUATION SCREEN'             
         LA    R2,CPQTABH                                                       
         OI    CPQFILH+6,X'81'                                                  
         SPACE 1                                                                
EXIT     OI    6(R2),X'40'                                                      
         XMOD1 1                                                                
         EJECT                                                                  
*                  S/R TO APPLY FILTERS VIA FILTER TABLE                        
*                  ON EXIT CC=EQU IF RECORD NOT REQUIRED                        
         SPACE 3                                                                
FILTER   NTR1  BASE=ABASE                                                       
         L     R8,A2NDBASE                                                      
         LA    R6,FILTAB                                                        
         USING FTABD,R6                                                         
         SPACE 1                                                                
F1       CLI   0(R6),X'FF'         END OF FILTER TABLE                          
         BE    FEND2                                                            
         MVC   FULL,FTABELMT                                                    
         L     R4,FULL                                                          
         L     R4,0(R4)                                                         
         LTR   R4,R4                                                            
         BNZ   F2                  IF ELEMENT CONTAINING FILTER DOES            
         CLI   FTABSIGN,C'N'       NOT EXIST AND FILTER IS NEGATIVE             
         BE    F4                  ASSUME IT IS SATISFIED                       
         B     FEND1                                                            
         SPACE 1                                                                
F2       MVC   HALF,FTABDISP                                                    
         AH    R4,HALF             R4 = FIELD ADDRESS                           
         ZIC   R5,FTABLEN          R5 = FIELD LENGTH                            
         LA    R3,FTABVAL          R3 = A(MASK OR COMPARE VALUE)                
         MVC   FULL,FBRANCH                                                     
         SR    R1,R1                                                            
         SPACE 1                                                                
         CLI   FTABMARK,C'C'       COMPARE VALUE (NOT MASK)                     
         BNE   F3                                                               
         OC    FTABSR,FTABSR       CALL CONVERSION S/R IF REQUIRED              
         BZ    *+10                                                             
         L     RF,FTABSR                                                        
         BASR  RE,RF                                                            
         CLI   FTABSIGN,C'N'       SWITCH EXECUTED BRANCH FROM 'BE' TO          
         BE    *+8                 'BNE' IF POSITIVE                            
         XI    FULL+1,X'F0'                                                     
         BCTR  R5,0                                                             
         EX    R5,FCOMPARE                                                      
         EX    R1,FULL                                                          
         B     F4                                                               
         SPACE 1                                                                
F3       CLI   FTABSIGN,C'P'       MASK VALUE (NOT COMPARE)                     
         BE    *+8                                                              
         XI    FULL+1,X'F0'        SWITCH EXECUTED BRANCH FROM 'BZ' TO          
         ZIC   R7,0(R3)            'BNZ' IF NEGATIVE                            
         EX    R7,FTEST                                                         
         EX    R1,FULL                                                          
         SPACE 1                                                                
F4       LA    R6,L'FILTAB(R6)     BUMP TO NEXT FILTER                          
         B     F1                                                               
         SPACE 1                                                                
FBRANCH  BE    FEND1               EXECUTED INSTRUCTIONS                        
FCOMPARE CLC   0(0,R4),0(R3)                                                    
FTEST    TM    0(R4),0                                                          
         SPACE 1                                                                
FEND1    SR    RB,RB               CC=EQU FOR SKIP THIS RECORD                  
FEND2    LTR   RB,RB                  NEQ FOR RECORD REQUIRED                   
         XIT1                                                                   
         SPACE 3                                                                
*                  CONVERSION SUBROUTINES FOR FILTER FIELDS                     
*                  ON ENTRY R4 = A(FILTER FIELD)                                
*                  ON EXIT  R4 = A(CONVERTED FILTER FIELD)                      
         SPACE 2                                                                
DAYPTCON NTR1                      CONVERT DAY-PART CODE TO ALPHA               
         LR    R5,R4                                                            
         LA    R4,WORK                                                          
         MVC   WORK(3),SPACES                                                   
         L     R3,ADPLUTAB                                                      
         SPACE 1                                                                
DYP1     CLI   0(R3),X'FF'                                                      
         BE    CONVXIT                                                          
         CLC   0(1,R3),0(R5)                                                    
         BE    DYP2                                                             
         LA    R3,9(R3)                                                         
         B     DYP1                                                             
         SPACE 1                                                                
DYP2     MVC   0(3,R4),5(R3)                                                    
         B     CONVXIT                                                          
         SPACE 3                                                                
SPOTLCON NTR1                      CONVERT SPOT LENGTH TO EBCDIC                
         LR    R5,R4                                                            
         LA    R4,WORK                                                          
         EDIT  (1,0(R5)),(3,WORK),ALIGN=LEFT,WRK=WORK+20                        
         B     CONVXIT                                                          
         SPACE 1                                                                
CONVXIT  XIT1  REGS=(R4)                                                        
         EJECT                                                                  
*                  S/R TO FORMAT A DISPLAY SCREEN FROMA MATRIX IN               
*                  GLOBAL W/S USING MAINTAB, A TABLE OF FORMATTING              
*                  INSTRUCTIONS IN GLOBAL W/S.                                  
         SPACE 3                                                                
FORMAT   NTR1  BASE=ABASE                                                       
         L     R8,A2NDBASE                                                      
         LA    R2,CPQDAT1H         R2 = SCREEN LINE POINTER                     
         MVC   8(L'CPQDAT1,R2),HEADING1                                         
         OI    6(R2),X'80'                                                      
         LA    R2,L'CPQDAT1H+L'CPQDAT1(R2)                                      
         MVI   LINE,1                                                           
         SPACE 1                                                                
         CLI   NOHEDLNS,1                                                       
         BE    FO1                                                              
         MVC   8(L'CPQDAT1,R2),HEADING2                                         
         OI    6(R2),X'80'                                                      
         LA    R2,L'CPQDAT1H+L'CPQDAT1(R2)                                      
         MVI   LINE,2                                                           
         SPACE 2                                                                
FO1      LA    R6,MATRIX           R6 = MATRIX POINTER                          
         USING MATRIXD,R6                                                       
         XC    LSTDAYPT(2),LSTDAYPT                                             
         MVI   TWOUP,0                                                          
         SPACE 1                                                                
FO2      CLI   MATDAYPT,X'FF'      END OF MATRIX                                
         BE    FO11                                                             
         OC    MATVALA(8),MATVALA NO VALUES IN THIS MATRIX ENTRY                
         BZ    FO10                                                             
         CLC   MATDAYPT(2),LSTDAYPT                                             
         BE    FO7                                                              
         ZIC   R5,LINE             NEW SCREEN LINE FOR NEW DAY-PART/LEN         
         LA    R5,1(R5)                                                         
         STC   R5,LINE                                                          
         CH    R5,=H'17'                                                        
         BNH   *+12                                                             
         BAS   RE,TWOUPCHK         CAN WE GO 2 OR 3-UP?                         
         BZ    FO11                NO                                           
         LA    R2,L'CPQDAT1H+L'CPQDAT1(R2)                                      
         OI    6(R2),X'80'                                                      
         ZIC   R3,TWOUP            R3 = POINTER TO 1ST DISPLAY FIELD IN         
         LA    R3,8(R2,R3)              CURRENT LINE                            
         CLC   MATDAYPT,LSTDAYPT                                                
         BE    FO6                                                              
         SPACE 1                                                                
         L     R4,ADPLUTAB         CHANGE OF DAY-PART. LOOK UP NAME             
FO3      CLI   0(R4),X'FF'                                                      
         BE    FO4                                                              
         CLC   MATDAYPT,0(R4)                                                   
         BE    FO5                                                              
         LA    R4,9(R4)                                                         
         B     FO3                                                              
FO4      MVC   0(3,R3),=C'???'     DAY-PART NOT IN LOOK-UP TABLE                
         B     FO6                                                              
FO5      MVC   0(3,R3),5(R4)       MOVE DAY-PART NAME INTO SCREEN LINE          
         SPACE 1                                                                
FO6      EDIT  MATLEN,(3,3(R3))    MOVE SPOT LENGTH IN                          
         MVC   LSTDAYPT(2),MATDAYPT                                             
         SPACE 2                                                                
FO7      LA    R9,MAINTAB          MATCH MATRIX ENTRY WITH MAINTAB ON           
         USING MAINTABD,R9         ID                                           
         SPACE 1                                                                
FO8      CLI   MAINID,X'FF'        CRASH IF NO MATCH                            
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   MAINID,MATID                                                     
         BE    FO9                                                              
         LA    R9,L'MAINTAB(R9)                                                 
         B     FO8                                                              
         SPACE 1                                                                
FO9      OC    MAINSRDS,MAINSRDS   CALL S/R TO EDIT VALUE                       
         BZ    FO9A                                                             
         MVC   FULL,MAINSRDS                                                    
         L     RF,FULL                                                          
         B     *+8                                                              
FO9A     L     RF,AFORMTSR                                                      
         GOTO1 (RF)                                                             
         SPACE 1                                                                
FO10     LA    R6,L'MATRIX(R6)     BUMP TO NEXT MATRIX ENTRY                    
         B     FO2                                                              
FO11     XIT1                                                                   
         SPACE 3                                                                
*                  S/R TO HANDLE SCREEN-FULL CONDITION                          
*                  IF REPORT TYPE IS GUIDE WITHOUT MENU 2 OR 3-UP               
*                   DISPLAY IS ORGANISED                                        
*                  OTHERWISE UNDISPLAYED PART OF MATRIX IS SAVED AND            
*                   'NEXT' IS SET TO 1.                                         
*                  ON EXIT CC=EQU IF NO MORE CAN BE DISPLAYED                   
         SPACE 3                                                                
TWOUPCHK NTR1                                                                   
         CLI   REPTYPE,3                                                        
         BNE   TWONO                                                            
         CLI   SAVEMENU,0                                                       
         BNE   TWONO                                                            
         ZIC   R5,TWOUP                                                         
         LA    R5,40(R5)                                                        
         CH    R5,=H'40'                                                        
         BH    TWONO                                                            
         STC   R5,TWOUP                                                         
         MVC   CPQDAT1+40(27),CPQDAT1   PROPAGATE HEADINGS                      
         XC    LSTDAYPT(2),LSTDAYPT                                             
         LA    R2,CPQDAT2H                                                      
         MVI   LINE,2                                                           
         CLI   NOHEDLNS,1                                                       
         BE    TWO5                                                             
         LA    R2,CPQDAT3H                                                      
         MVI   LINE,3                                                           
         MVC   CPQDAT2+40(27),CPQDAT2                                           
         B     TWO5                                                             
         SPACE 2                                                                
TWONO    GOTO1 ARDIR               READ ANOTHER TWA INTO TIA                    
         L     R1,ATIA                                                          
         USING TC03SAVX,R1                                                      
         L     R5,AENDMTRX                                                      
         LA    R4,L'MATRIX                                                      
         LA    R2,SAVEMTRX                                                      
         LA    R3,SAVEMXND                                                      
         SPACE 1                                                                
TWO1     MVC   0(L'MATRIX,R2),0(R6)                                             
         LA    R2,L'MATRIX(R2)                                                  
         CR    R2,R3               ROOM TO SAVE MORE ?                          
         BNL   TWO2                                                             
         BXLE  R6,R4,TWO1                                                       
         B     TWO3                                                             
         SPACE 1                                                                
TWO2     MVI   0(R2),X'FF'                                                      
         MVI   FERN,TOOLARGE                                                    
         LA    R2,L'MATRIX(R2)                                                  
         SPACE 1                                                                
TWO3     LA    R3,SAVEMTRX                                                      
         SR    R2,R3                                                            
         STH   R2,SAVEMXLN         LENGTH OF SAVED MATRIX                       
         MVC   SAVETAB,MAINTAB     SAVE MAINTAB                                 
         MVI   NEXT,1                                                           
         GOTO1 AWRITE                                                           
TWO4     SR    RB,RB                                                            
         SPACE 1                                                                
TWO5     LTR   RB,RB                                                            
         XIT1  REGS=(R2)                                                        
         EJECT                                                                  
*                  MATRIX UPDATE SUBROUTINES FOR ALL DATA TYPES                 
*                  ON ENTRY R5 = A(PERFORMANCE ELEMENT)                         
*                           R6 = A(MATRIX ENTRY TO BE UPDATED)                  
         SPACE 3                                                                
         USING CPPERFD,R5                                                       
         USING MATRIXD,R6                                                       
         SPACE 1                                                                
CPPUPD   NTR1  BASE=ABASE          CONVERT CASH & POINTS TO BINARY              
         L     R8,A2NDBASE         AND ADD TO MATVALA & MATVALB                 
         MVC   HALF,CPPCASH                                                     
         LH    R1,HALF                                                          
         XUNPK                                                                  
         LR    RA,R1                                                            
         MVC   HALF,CPPOINTS                                                    
         LH    R1,HALF                                                          
         XUNPK                                                                  
         LR    R5,R1                                                            
         B     UPCOMMN1                                                         
         SPACE 1                                                                
CPMUPD   NTR1  BASE=ABASE          CONVERT CASH & IMPS TO BINARY                
         L     R8,A2NDBASE         AND ADD TO MATVALA & MATVALB                 
         MVC   HALF,CPPCASH                                                     
         LH    R1,HALF                                                          
         XUNPK                                                                  
         LR    RA,R1                                                            
         MVC   HALF,CPPIMPS                                                     
         LH    R1,HALF                                                          
         XUNPK                                                                  
         LR    R5,R1                                                            
         B     UPCOMMN1                                                         
         SPACE 1                                                                
CPSUPD   NTR1  BASE=ABASE          CONVERT CASH & SPOTS TO BINARY               
         L     R8,A2NDBASE         AND ADD TO MATVALA & MATVALB                 
         MVC   HALF,CPPCASH                                                     
         LH    R1,HALF                                                          
         XUNPK                                                                  
         LR    RA,R1                                                            
CPS1     MVC   HALF,CPPSPOTS                                                    
         LH    R1,HALF                                                          
         XUNPK                                                                  
         LR    R5,R1                                                            
         B     UPCOMMN1                                                         
         SPACE 1                                                                
PPSUPD   NTR1  BASE=ABASE          CONVERT POINTS & SPOTS TO BINARY             
         L     R8,A2NDBASE         AND ADD TO MATVALA & MATVALB                 
         MVC   HALF,CPPOINTS                                                    
         LH    R1,HALF                                                          
         XUNPK                                                                  
         LR    RA,R1                                                            
         B     CPS1                                                             
         SPACE 1                                                                
IPSUPD   NTR1  BASE=ABASE          CONVERT IMPS & SPOTS TO BINARY               
         L     R8,A2NDBASE         AND ADD TO MATVALA & MATVALB                 
         MVC   HALF,CPPIMPS                                                     
         LH    R1,HALF                                                          
         XUNPK                                                                  
         LR    RA,R1                                                            
         B     CPS1                                                             
         SPACE 1                                                                
COSTUPD  NTR1  BASE=ABASE          CONVERT COST TO BINARY                       
         L     R8,A2NDBASE         AND ADD TO MATVALA                           
         MVC   HALF,CPPCASH                                                     
COST1    LH    R1,HALF                                                          
         XUNPK                                                                  
         LR    RA,R1                                                            
         B     UPCOMMN2                                                         
         SPACE 1                                                                
SPTSUPD  NTR1  BASE=ABASE          CONVERT SPOTS TO BINARY                      
         L     R8,A2NDBASE         AND ADD TO MATVALA                           
         MVC   HALF,CPPSPOTS                                                    
         B     COST1                                                            
         SPACE 1                                                                
PNTSUPD  NTR1  BASE=ABASE          CONVERT POINTS TO BINARY                     
         L     R8,A2NDBASE         AND ADD TO MATVALA                           
         MVC   HALF,CPPOINTS                                                    
         B     COST1                                                            
         SPACE 1                                                                
IMPSUPD  NTR1  BASE=ABASE          CONVERT IMPS TO BINARY                       
         L     R8,A2NDBASE         AND ADD TO MATVALA                           
         MVC   HALF,CPPIMPS                                                     
         B     COST1                                                            
         SPACE 3                                                                
UPCOMMN1 A     R5,MATVALB          COMMON CODE TO ADD VALUES IN                 
         ST    R5,MATVALB                                                       
         SPACE 1                                                                
UPCOMMN2 A     RA,MATVALA                                                       
         ST    RA,MATVALA                                                       
         XIT1                                                                   
         SPACE 1                                                                
         DROP  R5                                                               
         EJECT                                                                  
*                  MATRIX DISPLAY SUBROUTINES FOR ALL DATA TYPES                
*                  ON ENTRY R3 = A(SCREEN LINE)-MODDED FOR 2/3-UP               
*                           R6 = A(MATRIX ENTRY)                                
*                           R9 = A(MAINTAB ENTRY)-CONTAINS COL.NO.&LNTH         
         SPACE 3                                                                
         USING MAINTABD,R9                                                      
         USING MATRIXD,R6                                                       
         SPACE 1                                                                
CPPDISP  NTR1  BASE=ABASE          COST(A) OVER POINTS(B) TO 2 PLACES           
         L     R8,A2NDBASE                                                      
         LA    R7,3                3 DECIMAL PLACES                             
CPP1     L     R1,MATVALA                                                       
         BAS   RE,EQUIVAL                                                       
         L     R5,MATVALB                                                       
         BAS   RE,DIVIDE                                                        
         BCTR  R7,0                SHIFT DECIMAL PLACE                          
         BAS   RE,POSVAL                                                        
         BZ    CPP1                WONT FIT SO REDUCE DECIMAL PLACES            
         XIT1                                                                   
         SPACE 1                                                                
CPMDISP  NTR1  BASE=ABASE          COST(A) OVER IMPS(B) TO 2 PLACES             
         L     R8,A2NDBASE                                                      
         LA    R7,2                                                             
CPM1     L     R5,MATVALB                                                       
         L     R1,MATVALA                                                       
         BAS   RE,EQUIVAL                                                       
         BAS   RE,DIVIDE                                                        
         BAS   RE,POSVAL                                                        
         BNZ   CPMX                                                             
         BCTR  R7,0                WONT FIT SO REDUCE DECIMAL PLACES            
         B     CPM1                                                             
CPMX     XIT1                                                                   
         SPACE 1                                                                
NILDPDS  NTR1  BASE=ABASE          VALUE A OVER VALUE B TO NO DECIMALS          
         L     R8,A2NDBASE                                                      
         SR    R7,R7               NO DECIMAL PLACES                            
         L     R5,MATVALB                                                       
         L     1,MATVALA                                                        
         BAS   RE,DIVIDE                                                        
         BAS   RE,POSVAL                                                        
         XIT1                                                                   
         SPACE 1                                                                
PPSDISP  NTR1  BASE=ABASE          POINTS(A) OVER SPOTS(B) TO 1 PLACE           
         L     R8,A2NDBASE                                                      
         SR    R7,R7                                                            
         L     R1,MATVALA                                                       
         L     R5,MATVALB                                                       
         BAS   RE,DIVIDE                                                        
         LA    R7,1                SHIFT DECIMAL PLACE                          
         BAS   RE,POSVAL                                                        
         XIT1                                                                   
         SPACE 1                                                                
POINTDS  NTR1  BASE=ABASE                                                       
         L     R8,A2NDBASE                                                      
         L     R1,MATVALA                                                       
         LA    R7,1                                                             
         BAS   RE,POSVAL                                                        
         XIT1                                                                   
         SPACE 1                                                                
INTEGDS  NTR1  BASE=ABASE          VALUE A AS AN INTEGER                        
         L     R8,A2NDBASE                                                      
         L     R1,MATVALA                                                       
         SR    R7,R7                                                            
         BAS   RE,POSVAL                                                        
         XIT1                                                                   
         SPACE 3                                                                
*                  S-R TO APPLY EQUIVALENCY FACTORS FROM TABLE                  
*                  ON ENTRY R1       = APPLICAND BEFORE                         
*                           EQUIFCTS = TABLE OF SPOT LENGTHS       -CL1         
*                                             & EQUIVALENCY FACTORS-CL2         
*                  ON EXIT  R1       = APPLICAND AFTER                          
         SPACE 1                                                                
EQUIVAL  NTR1                                                                   
         LA   R5,EQUIFCTS                                                       
EQ1      CLC   MATLEN,0(R5)                                                     
         BE    EQ2                                                              
         LA    R5,L'EQUIFCTS(R5)                                                
         B     EQ1                                                              
EQ2      CLC   1(2,R5),=H'1000'                                                 
         BE    EQUIX                                                            
         MVC   HALF,1(R5)                                                       
         LH    R4,HALF                                                          
         SR    R0,R0                                                            
         M     R0,=F'1000'                                                      
         SLDA  R0,1                                                             
         DR    R0,R4                                                            
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
EQUIX    XIT1  REGS=(R1)                                                        
         SPACE 3                                                                
*                  S-R TO DIVIDE A BY B AND ROUND TO N DECIMAL PLACES           
*                  ON ENTRY R1      = DIVIDEND                                  
*                           R5      = DIVISOR                                   
*                           R7      = NO OF DECIMAL PLACES          )           
*                  ON EXIT  R1      = QUOTIENT IN BINARY TO N DECIMAL           
*                                     PLACES                                    
         SPACE 2                                                                
DIVIDE   NTR1                                                                   
         LTR   R5,R5               ZERO DIVISOR                                 
         BNZ   *+10                                                             
         SR    R1,R1                                                            
         B     DIVXIT                                                           
         SR    R0,R0                                                            
         LA    R2,1                                                             
         LA    R7,1(R7)                                                         
         MH    R2,=H'10'                                                        
         BCT   R7,*-4                                                           
         MR    R0,R2                                                            
         DR    R0,R5                                                            
         AH    R1,=H'5'                                                         
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
DIVXIT   XIT1  REGS=(R1)                                                        
         SPACE 2                                                                
*                  S-R TO EDIT A BINARY VALUE TO N DECIMAL PLACES               
*                  AND POSITION IT ON THE SCREEN                                
*                  ON ENTRY R1      = BINARY VALUE                              
*                           R3      = A(SCREEN LINE)-MODDED FOR 2/3 UP          
*                           R7      = NO OF DECIMAL PLACES                      
*                           R9      = A(MAINTAB ENTRY)                          
*                  ON EXIT  CC      = EQU IF PRECISION MUST & CAN BE            
*                                     REDUCED                                   
         SPACE 2                                                                
POSVAL   NTR1                                                                   
         EDIT  (R1),(10,WORK),WRK=WORK+20                                       
         LTR   R7,R7               ANY DECIMAL PLACES                           
         BZ    POS2                                                             
         LA    R2,8                INSERT DECIMAL POINT                         
         SR    R2,R7                                                            
         EX    R2,EDITMOVE                                                      
         LA    R2,WORK+9                                                        
         SR    R2,R7                                                            
         MVI   0(R2),C'.'                                                       
         BCTR  R7,0                                                             
         EX    R7,EDITORC          NO SPACES AFTER DECIMAL POINT                
         SPACE 1                                                                
POS2     LA    R4,WORK+9                                                        
         ZIC   R5,MAINSCRL                                                      
         SR    R4,R5               R4=A(BYTE BEFORE 1ST DISPLAY BYTE)           
POS3     CLI   0(R4),C' '          VALUE TOO LARGE                              
         BNH   POS6                                                             
         LTR   R7,R7               IF THERE ARE DECIMAL PLACES SET              
         BM    POS5                CC TO EQUAL & EXIT                           
         SR    RB,RB                                                            
         B     POS7                                                             
         SPACE 1                                                                
POS5     MVI   WORK,C'*'           DISPLAY ASTERISKS IF VALUE TOO LARGE         
         MVC   WORK+1(9),WORK                                                   
         SPACE 1                                                                
POS6     LA    R4,1(R4)            R4=A(DISPLAY VALUE)                          
         BCTR  R5,0                                                             
         ZIC   R1,MAINSCRC         R1=START COLUMN NUMBER                       
         AR    R1,R3                                                            
         BCTR  R1,0                                                             
         BCTR  R1,0                R1=A(DISPLAY POSITION)                       
         EX    R5,PVMOVE                                                        
         SPACE 1                                                                
POS7     LTR   RB,RB                                                            
         XIT1                                                                   
EDITMOVE MVC   WORK(0),WORK+1                                                   
EDITORC  OC    1(0,R2),=C'000'                                                  
PVMOVE   MVC   0(0,R1),0(R4)                                                    
         EJECT                                                                  
*                  SUBROUTINE TO ADD OR FIND A MATRIX ENTRY VIA BINSRCH         
*                  ON ENTRY R6   = A(RECORD TO BE ADDED OR FOUND)               
*                  ON EXIT  CC   = EQU IF MATRIX IS FULL                        
*                           DMCB = A(MATRIX ENTRY IN MATRIX)                    
         SPACE 3                                                                
SEARCH   NTR1  BASE=ABASE                                                       
         L     R8,A2NDBASE                                                      
         LA    R3,L'MATRIX                                                      
         L     R4,RECNUM                                                        
         L     R5,MAXRECS                                                       
         GOTO1 VBINSRCH,DMCB,(1,(R6)),MATRIX,(R4),(R3),(0,3),(R5)               
         MVC   RECNUM(4),DMCB+8                                                 
         OC    DMCB,DMCB                                                        
         BNZ   SEARCHX                                                          
         MVI   FERN,TOOLARGE                                                    
SEARCHX  XIT1                                                                   
         EJECT                                                                  
*                  DATAMANAGER ROUTINES                                         
         SPACE 3                                                                
HIGH     NTR1  BASE=ABASE          CPFILE READ HIGH                             
         L     R8,A2NDBASE                                                      
         LA    R2,=C'DMRDHI'                                                    
         B     DM1                                                              
         SPACE 1                                                                
READ     NTR1  BASE=ABASE          CPFILE READ (CLIENT RECORDS)                 
         L     R8,A2NDBASE                                                      
         LA    R2,=C'DMREAD'                                                    
         SPACE 1                                                                
DM1      LA    R3,=C'CPFILE'                                                    
         LA    R4,KEY                                                           
         LA    R5,IO                                                            
         B     DMALL                                                            
         SPACE 1                                                                
RDIR     NTR1  BASE=ABASE          TEMPSTR READ OF TWA/P1 FOR ADDITIONL         
         L     R8,A2NDBASE                                                      
         LA    R2,=C'DMRDIR'       SAVE STORAGE INTO TIA                        
         B     DM2                                                              
         SPACE 1                                                                
WRITE    NTR1  BASE=ABASE          TEMPSTR WRITE OF TWA/P1 FROM TIA             
         L     R8,A2NDBASE                                                      
         LA    R2,=C'DMWRT'                                                     
         SPACE 1                                                                
DM2      LA    R3,=C'TEMPSTR'                                                   
         LH    R4,TERMINAL                                                      
         ICM   R4,8,=X'01'                                                      
         L     R5,ATIA                                                          
         SPACE 1                                                                
DMALL    STM   R2,R5,DMCB          COMMON CODE                                  
         GOTO1 VDATAMGR,DMCB                                                    
         TM    DMCB+8,X'FF'                                                     
         BZ    DM3                                                              
         TM    DMCB+8,X'10'                                                     
         BO    *+8                                                              
         MVI   FERN,0                                                           
         L     RD,AREGSAVE                                                      
         LM    RE,RC,12(RD)                                                     
         B     ERROR                                                            
         SPACE 1                                                                
DM3      CLC   0(6,R2),=C'DMRDHI'  IF CPFILE READ, SAVE EQUIVALENCY             
         BNE   DMEXIT              FACTOR  IN A TABLE                           
         LA    R4,IO                                                            
         USING CPKEYD,R4                                                        
         LA    R5,EQUIFCTS                                                      
         SPACE 1                                                                
DM4      CLC   CPKSPTLN,0(R5)                                                   
         BE    DMEXIT                                                           
         CLI   0(R5),0                                                          
         BE    DM5                                                              
         LA    R5,L'EQUIFCTS(R5)                                                
         B     DM4                                                              
         SPACE 1                                                                
DM5      MVC   0(1,R5),CPKSPTLN                                                 
         LA    R4,CPRECORD                                                      
         USING CPDATAD,R4                                                       
         MVC   1(2,R5),=H'1000'                                                 
         CLI   0(R4),X'02'                                                      
         BNE   DMEXIT                                                           
         MVC   1(2,R5),CPDEQUIV                                                 
         SPACE 1                                                                
DMEXIT   XIT1                                                                   
         SPACE 1                                                                
         DROP  R4                                                               
         EJECT                                                                  
*                  ERROR HANDLING                                               
         SPACE 3                                                                
ERROR    CLI   FERN,X'FE'          MESSAGE SUPPLIED                             
         BE    ERRORY                                                           
         ZIC   R4,FERN                                                          
         GOTO1 VGETMSG,DMCB+12,((R4),SPACES),(12,DMCB),VDATAMGR                 
         CLI   FNDX,0              MULTI-FIELD ERROR ?                          
         BE    ERRORX                                                           
         ZIC   R5,0(R1)            GET MESSAGE LENGTH                           
         LA    R6,SPACES(R5)       POINT TO END OF MESSAGE                      
         LA    R5,11(R5)                                                        
         CH    R5,=H'60'           TEST IF ROOM TO EXTEND                       
         BH    ERRORX                                                           
         MVC   0(11,R6),=C' - FIELD#NN'                                         
         IC    R5,FNDX                                                          
         CVD   R5,DUB                                                           
         UNPK  9(2,R6),DUB                                                      
         OI    10(R6),X'F0'                                                     
         SPACE 1                                                                
ERRORX   MVC   CPQHEAD,SPACES                                                   
         SPACE 1                                                                
ERRORY   MVI   NEXT,0                                                           
         B     EXIT                                                             
         EJECT                                                                  
*                  TABLES USED BY ROOT PHASE                                    
         SPACE 3                                                                
ACOMMON  DC    A(HIGH)             COMMON SUBROUTINE ADDRESSES                  
         DC    A(READ)                                                          
         DC    A(RDIR)                                                          
         DC    A(WRITE)                                                         
         DC    A(FILTER)                                                        
         DC    A(FORMAT)                                                        
         DC    A(CPPUPD)                                                        
         DC    A(CPMUPD)                                                        
         DC    A(CPSUPD)                                                        
         DC    A(PPSUPD)                                                        
         DC    A(IPSUPD)                                                        
         DC    A(COSTUPD)                                                       
         DC    A(SPTSUPD)                                                       
         DC    A(PNTSUPD)                                                       
         DC    A(IMPSUPD)                                                       
         DC    A(CPPDISP)                                                       
         DC    A(CPMDISP)                                                       
         DC    A(PPSDISP)                                                       
         DC    A(NILDPDS)                                                       
         DC    A(POINTDS)                                                       
         DC    A(INTEGDS)                                                       
         DC    A(SEARCH)                                                        
         DC    V(DPLUTAB)                                                       
         SPACE 3                                                                
REPTAB   DS    0CL8                TABLE OF REPORT TYPES                        
*                                  COVERED BY DSECT REPTABD                     
*                                  REPORT NAME/NUMBER/COMPATIBLE DATA           
*                                                     TYPE MASK                 
         DC    C'GUIDE ',X'03F8'                                                
         DC    C'TREND ',X'04F8'                                                
         DC    C'DETAIL',X'0500'                                                
         DC    X'FF'                                                            
         SPACE 3                                                                
DATATAB  DS    0CL8                TABLE OF DATA TYPES                          
*                                  COVERED BY DSECT DATTABD                     
*                                  NAME/NUMBER/DISPLACEMENT INTO GWS OF         
*                                  POINTERS TO A(UPDATE & DISPLAY S/RS)         
         DC    C'CPP',X'80',AL2(ACPPUPD-GWS),AL2(ACPPDISP-GWS)                  
         DC    C'CPM',X'40',AL2(ACPMUPD-GWS),AL2(ACPMDISP-GWS)                  
         DC    C'CPS',X'20',AL2(ACPSUPD-GWS),AL2(ANILDPDS-GWS)                  
         DC    C'PPS',X'10',AL2(APPSUPD-GWS),AL2(APPSDISP-GWS)                  
         DC    C'IPS',X'08',AL2(AIPSUPD-GWS),AL2(ANILDPDS-GWS)                  
         DC    X'FF'                                                            
         SPACE 3                                                                
MENULIST DS    0CL1                LIST OF VALID MENU NUMBERS                   
*&&UK                                                                           
         DC    X'01'                                                            
*&&                                                                             
*&&US                                                                           
         DC    X'010203040507'                                                  
*&&                                                                             
         DC    X'FF'                                                            
         SPACE 3                                                                
         DS    0F                                                               
FILTERS  DS    0CL16               TABLE OF AVAILABLE FILTERS                   
*                                  COVERED BY DSECT FILTD (QV FOR DF)           
         SPACE 1                                                                
         DC    CL2'PT'             PROGRAM TYPE                                 
         DC    CL5' '                                                           
         DC    AL2(AKEY-GWS)                                                    
         DC    AL2(CPKPROG-CPKEYD)                                              
         DC    AL1(L'CPKPROG)                                                   
         DC    AL4(0)                                                           
         SPACE 1                                                                
         DC    CL2'DP'             DAY-PART                                     
         DC    CL5' '                                                           
         DC    AL2(AKEY-GWS)                                                    
         DC    AL2(CPKDAYPT-CPKEYD)                                             
         DC    AL1(3)                                                           
         DC    A(DAYPTCON)                                                      
         SPACE 1                                                                
         DC    CL2'SL'             SPOT LENGTH                                  
         DC    CL5' '                                                           
         DC    AL2(AKEY-GWS)                                                    
         DC    AL2(CPKSPTLN-CPKEYD)                                             
         DC    AL1(3)                                                           
         DC    A(SPOTLCON)                                                      
         SPACE 1                                                                
         DC    X'FF'                                                            
         SPACE 1                                                                
MAXRECS  DC    F'320'              MAX NUMBER OF 12-BYTE MATRIX ENTRIES         
*&&UK                                                                           
UKSTATS  DS    0CL12                                                            
*              NESTED INCLUDE FOR MEGENSTLST COVERED BY DSECT UKSTATSD          
         PRINT OFF                                                              
       ++INCLUDE MEGENSTLST                                                     
         PRINT ON                                                               
*&&                                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE CPGENDPLUT                                                     
         EJECT                                                                  
       ++INCLUDE CPINQDSECT                                                     
         EJECT                                                                  
*                  NESTED INCLUDES FOR CPGENFILE,DDCOMFACS & FATWA              
         PRINT OFF                                                              
       ++INCLUDE CPGENFILE                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FATWA                                                          
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006CPINQ00   05/01/02'                                      
         END                                                                    
