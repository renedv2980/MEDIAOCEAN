*          DATA SET PELEVLM    AT LEVEL 209 AS OF 07/17/07                      
*PHASE PELEVLMA                                                                 
*INCLUDE CARDS                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE DMUTLPE                                                                
*INCLUDE FATABOFF                                                               
*INCLUDE XSORT                                                                  
*                                                                               
         TITLE 'PERSON SYSTEM FACPAK LEVEL REPORT'                              
PELEVLM  CSECT                                                                  
         ENTRY SSB                                                              
         PRINT NOGEN                                                            
         NBASE WORKX-WORKD,**PELV**,RA,WORK=A(WORKC),CLEAR=YES                  
         USING WORKD,RC            RC=A(GLOBAL W/S)                             
*                                                                               
         L     R1,=A(IOAREA1-WORKD)                                             
         AR    R1,RC                                                            
         ST    R1,AIO1                                                          
*                                                                               
MAIN     BAS   RE,PRINTI           INIT PRINTING                                
         BAS   RE,INIT             OPEN FILES ECT                               
*                                                                               
         BAS   RE,READBOOK                                                      
         BAS   RE,BLDLEVEL                                                      
*                                                                               
         MVC   TITLE(50),TITLE1                                                 
         BAS   RE,PRINTT                                                        
         BAS   RE,REPORT1                                                       
*                                                                               
         MVC   TITLE(50),TITLE2                                                 
         BAS   RE,PRINTT                                                        
         BAS   RE,REPORT2                                                       
*                                                                               
         MVC   TITLE(50),TITLE3                                                 
         BAS   RE,PRINTT                                                        
         BAS   RE,REPORT3                                                       
*                                                                               
         MVC   FLAGWORD,=C'YYYY'   LOOK FOR UK/US SRC AND RELO                  
         MVC   TITLE(50),TITLE4                                                 
         BAS   RE,PRINTT                                                        
         BAS   RE,REPORT4                                                       
*                                                                               
         BAS   RE,CTRYSORT                                                      
*                                                                               
         MVC   FLAGWORD,=C'YNYN'   LOOK FOR UK SRC AND RELO                     
         MVC   TITLE(50),TITLE5                                                 
         BAS   RE,PRINTT                                                        
         BAS   RE,REPORT4                                                       
*                                                                               
         MVC   FLAGWORD,=C'NYNY'   LOOK FOR US SRC AND RELO                     
         MVC   TITLE(50),TITLE6                                                 
         BAS   RE,PRINTT                                                        
         BAS   RE,REPORT4                                                       
*                                                                               
         MVC   TITLE(50),TITLE7                                                 
         BAS   RE,PRINTT                                                        
         BAS   RE,REPORT9                                                       
*                                                                               
MAIN990  BAS   RE,CLOSE            CLOSE FILES ECT                              
*                                                                               
XBASE    XBASE                     PROG EXIT                                    
*                                                                               
EXITN    LTR   RB,RB               EXIT CC=NEQ                                  
         B     EXIT                                                             
EXITY    CR    RB,RB               EXIT CC=EQU                                  
EXIT     XIT1                                                                   
         EJECT                                                                  
*************************************************************                   
*        INITIALISATION / OPEN FILES                        *                   
*************************************************************                   
         SPACE 1                                                                
INIT     NTR1                                                                   
INIT010  L     R3,AIO1                                                          
         GOTO1 =V(CARDS),DMCB,(R3),=C'RE00'                                     
         CLC   =C'/*',0(R3)                                                     
         BE    INIT020                                                          
         LR    R1,R3                                                            
         BAS   RE,VALCARD                                                       
         B     INIT010                                                          
*                                                                               
INIT020  L     R1,=V(SSB)                                                       
*                                                                               
         L     RF,=V(DDSIO)        SET UP DDSIO                                 
         MVC   0(8,RF),DDSIO                                                    
*                                                                               
INIT030  L     R1,=V(UTL)          OPEN PER2 FILES                              
         MVI   4(R1),X'1E'                                                      
         GOTO1 =V(DATAMGR),DMCB,DMOPEN,PERSON,PFILES,AIO1                       
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         B     INIT990                                                          
*                                                                               
INIT050  L     R1,=V(UTL)          PER2 FILES                                   
         MVI   4(R1),X'1E'                                                      
*                                                                               
INIT990  B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        READ BOOK                                          *                   
*************************************************************                   
         SPACE 1                                                                
READBOOK NTR1                                                                   
*                                                                               
         USING PEPANLD,PERKEY                                                   
         MVI   PELPANID,PELPANIQ                                                
         MVC   PELBOOK,BOOK                                                     
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,DMRDHI,PERDIR,PERKEY,PERKEY                     
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,GETREC,PERFIL,PELDA,AIO1,IOW                    
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,AIO1             BUILD LIST OF INCLUDE BOOKS                  
         LA    R2,PELDATA-PEPANLD(R2)                                           
         LA    R1,INCLAREA                                                      
READ051  SR    R0,R0                                                            
         CLI   0(R2),0                                                          
         BE    READ052                                                          
         CLI   0(R2),C'I'                                                       
         BNE   *+14                                                             
         MVC   0(8,R1),2(R2)       CL8 RMBOOK                                   
         LA    R1,8(R1)                                                         
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     READ051                                                          
*                                                                               
READ052  XC    0(8,R1),0(R1)                                                    
*                                                                               
         LA    R2,INCLAREA         READ PASSIVES FOR SOURCE                     
         L     R3,=A(SORCAREA-WORKD)                                            
         AR    R3,RC                                                            
         USING PEPANLPD,PERKEY                                                  
         XC    PEPANLPK,PEPANLPK                                                
         MVI   PELPANLD,PELPANLQ                                                
READ053  MVC   PELRM,0(R2)                                                      
         XC    PELSRC,PELSRC                                                    
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,DMRDHI,PERDIR,PERKEY,PERKEY                     
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   0(10,R3),DOTS       COPY SOURCE TO TABLE                         
         CLC   PELRM,0(R2)         MUST FIND ONE                                
         BNE   READ055                                                          
*                                                                               
READ054  MVC   0(10,R3),PELSRC     COPY SOURCE TO TABLE                         
READ055  LA    R3,10(R3)                                                        
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,DMRSEQ,PERDIR,PERKEY,PERKEY                     
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   PELRM,0(R2)         ANY MORE SOURCE BOOKS                        
         BE    READ054                                                          
*                                                                               
         LA    R2,8(R2)                                                         
         CLI   0(R2),0             EOT?                                         
         BNE   READ053                                                          
*                                                                               
READBX   B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        BUILD LEVEL TABLE                                  *                   
*************************************************************                   
         SPACE 1                                                                
BLDLEVEL NTR1                                                                   
*                                                                               
         LA    R2,KEYBUFF                                                       
         L     R3,=A(SORCAREA-WORKD)                                            
         AR    R3,RC                                                            
BLDLEV01 XC    PERKEY,PERKEY                                                    
         MVI   PELPANID,PELPANIQ                                                
         MVC   PELBOOK,0(R3)                                                    
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,DMRDHI,PERDIR,PERKEY,PERKEY                     
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
BLDLEV05 CLC   PELBOOK,0(R3)                                                    
         BNE   BLDLEV50                                                         
*                                                                               
         CLI   PELTYP,C'P'         IGNORE PROG LIBS                             
         BE    BLDLEV20                                                         
         CLI   PELTYP,C'L'         FILTER LOAD LIBS                             
         BNE   BLDLEV10                                                         
*                                                                               
         CLC   PELLIB,=C'TEST'                                                  
         BNE   *+8                                                              
         LA    R1,PHASET                                                        
         CLC   PELLIB,=C'LOAD'                                                  
         BNE   *+8                                                              
         LA    R1,PHASEL                                                        
*                                                                               
BLDLEV06 CLI   0(R1),C' '          NOT FOUND SO IGNORE                          
         BE    BLDLEV20                                                         
         CLI   0(R1),X'FF'         NOT FOUND SO IGNORE                          
         BE    BLDLEV20                                                         
*                                                                               
         CLC   0(8,R1),PELTEST                                                  
         BE    BLDLEV10                                                         
         CLC   0(8,R1),PELPROD                                                  
         BE    BLDLEV10                                                         
         LA    R1,8(R1)                                                         
         B     BLDLEV06                                                         
*                                                                               
BLDLEV10 MVC   0(36,R2),PERKEY                                                  
         LA    R2,36(R2)                                                        
         LA    R1,KEYBUFF                                                       
         A     R1,=A(KEYBUFFX-KEYBUFF)                                          
         CR    R2,R1                                                            
         BL    *+6                                                              
         DC    H'0'                                                             
*                                                                               
BLDLEV20 GOTO1 =V(DATAMGR),DMCB,DMRSEQ,PERDIR,PERKEY,PERKEY                     
         CLI   8(R1),0                                                          
         BE    BLDLEV05                                                         
         DC    H'0'                                                             
*                                                                               
BLDLEV50 LA    R3,10(R3)                                                        
         CLI   0(R3),0                                                          
         BNE   BLDLEV01                                                         
*                                                                               
BLDLEVX  B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        BUILD REPORT1 EXTRACT ANY PHASE VERSIONS  HUH!!    *                   
*************************************************************                   
         SPACE 1                                                                
         USING PLINED,PLINE                                                     
         USING PEPANLD,R2                                                       
REPORT1  NTR1                                                                   
*                                                                               
         LA    R2,KEYBUFF          BUILD REPORT FROM KEYBUFF                    
REP1010  CLI   PELTYP,C'A'                                                      
         BNE   REP1020                                                          
*                                                                               
         BAS   RE,REPORTL          PRINT LINE                                   
         MVC   0(36,R2),FFS        AND REMOVE FROM TABLE                        
*                                                                               
REP1020  LA    R2,36(R2)                                                        
         CLI   0(R2),0                                                          
         BNE   REP1010                                                          
*                                                                               
REPORT1X B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        BUILD REPORT2 EXTRACT ANY UNASSEMBLED SOURCE       *                   
*************************************************************                   
         SPACE 1                                                                
         USING PLINED,PLINE                                                     
         USING PEPANLD,R2                                                       
REPORT2  NTR1                                                                   
*                                                                               
         LA    R2,KEYBUFF          BUILD REPORT FROM KEYBUFF                    
REP2004  CLI   PELTYP,C'B'                                                      
         BNE   REP2020                                                          
*                                                                               
         MVC   KEYSAVE,0(R2)       LOOK FOR C TYPE (COMPILED)                   
         MVI   KEYSAVE+PELTYP-PEPANLD,C'C'                                      
         LR    R3,R2                                                            
*                                                                               
REP2005  LA    R3,36(R3)           IGNORE FFS                                   
         CLI   0(R3),X'FF'                                                      
         BE    REP2005                                                          
*                                                                               
         CLC   KEYSAVE(13),0(R3)   NOT FOUND SO PRINT AND REMOVE                
         BNE   REP2010                                                          
*                                                                               
         CLC   KEYSAVE,0(R3)       FOUND IT MOVE ON                             
         BNE   REP2020                                                          
         B     REP2005             OR KEEP LOOKING                              
*                                                                               
REP2010  BAS   RE,REPORTL          PRINT LINE                                   
         MVC   0(36,R2),FFS        AND REMOVE FROM TABLE                        
*                                                                               
REP2020  LA    R2,36(R2)                                                        
         CLI   0(R2),0                                                          
         BNE   REP2004                                                          
*                                                                               
REPORT2X B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        BUILD REPORT3 EXTRACT ANY ORPHANED RELO BOOKS      *                   
*************************************************************                   
         SPACE 1                                                                
         USING PLINED,PLINE                                                     
         USING PEPANLD,R2                                                       
REPORT3  NTR1                                                                   
*                                                                               
         LA    R2,KEYBUFF          BUILD REPORT FROM KEYBUFF                    
REP3004  CLI   PELTYP,C'C'                                                      
         BNE   REP3020                                                          
*                                                                               
         MVC   KEYSAVE,0(R2)       LOOK FOR B TYPE (RM SOURCE)                  
         MVI   KEYSAVE+PELTYP-PEPANLD,C'B'                                      
         LR    R3,R2                                                            
*                                                                               
REP3005  SHI   R3,36               IGNORE FFS                                   
         CLI   0(R3),X'FF'                                                      
         BE    REP3005                                                          
*                                                                               
         CLC   KEYSAVE(13),0(R3)   NOT FOUND SO PRINT AND REMOVE                
         BNE   REP3010                                                          
*                                                                               
         CLC   KEYSAVE,0(R3)       FOUND IT MOVE ON                             
         BNE   REP3020                                                          
         B     REP3005             OR KEEP LOOKING                              
*                                                                               
REP3010  BAS   RE,REPORTL          PRINT LINE                                   
         MVC   0(36,R2),FFS        AND REMOVE FROM TABLE                        
*                                                                               
REP3020  LA    R2,36(R2)                                                        
         CLI   0(R2),0                                                          
         BNE   REP3004                                                          
*                                                                               
REPORT3X B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        BUILD REPORT4 COMPLETE MODULES IN SYNC AND CURRENT *                   
*************************************************************                   
         SPACE 1                                                                
         USING PLINED,PLINE                                                     
         USING PEPANLD,R2                                                       
REPORT4  NTR1                                                                   
*                                                                               
         LA    R2,KEYBUFF          BUILD REPORT FROM KEYBUFF                    
         CLI   0(R2),X'FF'         IGNORE                                       
         BE    REP4015                                                          
*                                                                               
REP4005  MVC   KEYFLAGS,=C'NNNN'                                                
*                                                                               
         MVC   KEYSAVE,0(R2)       SAVE FIRST KEY                               
         CLI   0(R2),0                                                          
         BE    REPORT4X                                                         
         ST    R2,FULL             SAVE ITS ADDRESS TOO                         
*                                                                               
REP4010  CLC   KEYSAVE(13),0(R2)   SAME BOOK LEVEL DATE                         
         BNE   REP4050                                                          
*                                                                               
         CLC   PELTYP(6),=C'BKAPPL'                                             
         BNE   *+12                                                             
         MVI   KEYFLAG1,C'Y'       SET UK SOURCE FOUND                          
         B     REP4020                                                          
*                                                                               
         CLC   PELTYP(6),=C'BSAPPL'                                             
         BNE   *+12                                                             
         MVI   KEYFLAG2,C'Y'       SET US SOURCE FOUND                          
         B     REP4020                                                          
*                                                                               
         CLC   PELTYP(6),=C'CKAPPL'                                             
         BNE   *+12                                                             
         MVI   KEYFLAG3,C'Y'       SET UK RELO FOUND                            
         B     REP4020                                                          
*                                                                               
         CLC   PELTYP(6),=C'CSAPPL'                                             
         BNE   *+12                                                             
         MVI   KEYFLAG4,C'Y'       SET US RELO FOUND                            
         B     REP4020                                                          
*                                                                               
         CLI   PELTYP,C'L'         LOAD MODULES                                 
         BE    REP4020                                                          
*                                                                               
*                                  SOMETHING ELSE THEN                          
*                                                                               
REP4015  LA    R2,36(R2)                                                        
         CLI   0(R2),0             EOT                                          
         BE    REPORT4X                                                         
         CLI   0(R2),X'FF'         IGNORE                                       
         BE    REP4015                                                          
         CLC   PELBOOK,KEYSAVE     SKIP ALL FOR THIS BOOK                       
         BE    REP4015                                                          
         B     REP4005                                                          
*                                                                               
REP4020  LA    R2,36(R2)                                                        
         CLI   0(R2),X'FF'         IGNORE                                       
         BE    REP4020                                                          
         B     REP4010                                                          
*                                                                               
REP4050  CLC   KEYFLAGS,FLAGWORD   DID WE GET SOURCE AND RELO REQ'D             
         BNE   REP4005                                                          
*                                                                               
         LR    R3,R2               SAVE CURRENT ADDRESS                         
         L     R2,FULL             GET START ADDRESS                            
         BAS   RE,REPORTL          PRINT LINE                                   
REP4055  MVC   0(36,R2),FFS        AND REMOVE FROM TABLE                        
         LA    R2,36(R2)                                                        
         CR    R2,R3                                                            
         BL    REP4055             AND REMOVE THE REST                          
         B     REP4005                                                          
*                                                                               
REPORT4X B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        SORT BY COUNTRY                                    *                   
*************************************************************                   
         SPACE 1                                                                
CTRYSORT NTR1                                                                   
         LA    R2,KEYBUFF          BUILD REPORT FROM KEYBUFF                    
         XR    R3,R3                                                            
*                                                                               
CTRYS010 CLI   PELCTRY,C'K'        USE LOWER CASE TO SORT UK BOOK               
         BNE   *+10                                                             
         NC    PELBOOK,=X'BFBFBFBFBFBFBFBFBFBF'                                 
*                                                                               
         LA    R2,36(R2)                                                        
         LA    R3,1(R3)                                                         
         CLI   0(R2),0             EOT                                          
         BNE   CTRYS010                                                         
*                                                                               
         GOTO1 =V(XSORT),DMCB,KEYBUFF,(R3),36,36,0                              
*                                                                               
         LA    R2,KEYBUFF          BUILD REPORT FROM KEYBUFF                    
CTRYS020 OC    PELBOOK,=X'40404040404040404040'                                 
         LA    R2,36(R2)                                                        
         CLI   0(R2),0             EOT                                          
         BNE   CTRYS020                                                         
*                                                                               
CTRYSX   B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        BUILD REPORT9                                      *                   
*************************************************************                   
         SPACE 1                                                                
         USING PLINED,PLINE                                                     
         USING PEPANLD,R2                                                       
REPORT9  NTR1                                                                   
*                                                                               
         LA    R2,KEYBUFF          BUILD REPORT FROM KEYBUFF                    
         CLI   0(R2),X'FF'         IGNORE                                       
         BE    REP9020                                                          
*                                                                               
REP9010  MVC   KEYSAVE,0(R2)       PRINT LINE                                   
         BAS   RE,REPORTL          PRINT LINE                                   
*                                                                               
REP9020  LA    R2,36(R2)                                                        
         CLI   0(R2),0             EOT                                          
         BE    REPORT9X                                                         
         CLI   0(R2),X'FF'         IGNORE                                       
         BE    REP9020                                                          
         CLC   KEYSAVE(13),0(R2)                                                
         BE    REP9010                                                          
         BAS   RE,PRINTB                                                        
         B     REP9010                                                          
*                                                                               
REPORT9X B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        BUILD REPORT LINE                                  *                   
*************************************************************                   
         SPACE 1                                                                
         USING PLINED,PLINE                                                     
         USING PEPANLD,R2                                                       
REPORTL  NTR1                                                                   
*                                                                               
         MVC   PLINE,SPACES                                                     
         MVC   PLIBOOK,PELBOOK                                                  
*                                                                               
         MVC   HALF,PELDAT                                                      
         XC    HALF,=X'FFFF'                                                    
         GOTO1 =V(DATCON),DMCB,(2,HALF),(11,PLIDATE)                            
*                                                                               
         EDIT  (B1,PELLEV),(3,PLILEV),FILL=0                                    
*                                                                               
         MVC   PLITYPE,=C'BOOK    '                                             
         CLI   PELTYP,C'A'                                                      
         BE    REP010                                                           
         CLI   PELTYP,C'B'                                                      
         BE    REP010                                                           
         MVC   PLITYPE,=C'RELO    '                                             
         CLI   PELTYP,C'C'                                                      
         BE    REP010                                                           
         MVC   PLITYPE,=C'PHASE   '                                             
         CLI   PELTYP,C'L'                                                      
         BE    REP010                                                           
         DC    H'0'                                                             
*                                                                               
REP010   MVC   PLICTRY,=C'UK'                                                   
         CLI   PELCTRY,C'K'                                                     
         BE    *+10                                                             
         MVC   PLICTRY,=C'US'                                                   
*                                                                               
         MVC   PLILIB(4),=C'PAN.'                                               
         MVC   PLILIB+4(4),PELLIB                                               
         CLI   PELTYP,C'L'                                                      
         BNE   REP020                                                           
         MVC   PLILIB(4),PELLIB                                                 
         MVC   PLILIB+4(4),=C'LIB '                                             
*                                                                               
REP020   MVC   PLINAME,PELTEST                                                  
         MVC   PLINAME2,PELPROD                                                 
         BAS   RE,PRINTL                                                        
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        CLOSE FILES                                        *                   
*************************************************************                   
         SPACE 1                                                                
CLOSE    NTR1                                                                   
         GOTO1 =V(DATAMGR),DMCB,DMCLSE,PERSON,0,AIO1                            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                ERRORS ARE DEADLY                            
         B     EXITY                                                            
         EJECT                                                                  
*************************************************************                   
*        PARAMETER CARD CONSTANTS                           *                   
*************************************************************                   
         SPACE 1                                                                
DDSIO    DC     C'DDSIO   '                                                     
BOOK     DC     C'          '                                                   
*                                                                               
PHASET   DC     C'        '                                                     
         DC     C'        '                                                     
         DC     C'        '                                                     
         DC     C'        '                                                     
         DC     C'        '                                                     
         DC     C'        '                                                     
         DC     C'        '                                                     
         DC     C'        '                                                     
         DC     C'        '                                                     
         DC     C'        '                                                     
         DC     C'        '                                                     
         DC     C'        '                                                     
         DC     X'FF'                                                           
*                                                                               
PHASEL   DC     C'        '                                                     
         DC     C'        '                                                     
         DC     C'        '                                                     
         DC     C'        '                                                     
         DC     C'        '                                                     
         DC     C'        '                                                     
         DC     C'        '                                                     
         DC     C'        '                                                     
         DC     C'        '                                                     
         DC     C'        '                                                     
         DC     C'        '                                                     
         DC     C'        '                                                     
         DC     X'FF'                                                           
         EJECT                                                                  
************************************************************                    
*        VALIDATE PARAMETER CARDS                          *                    
************************************************************                    
*                                                                               
*        CL7'KEYWORD',AL1(KEYWRD LEN-1,OP LEN-1),X'FLAGS',AL3(OUTPUT)           
*                                                                               
*FLAGS   X'8000'                   A(OUTPUT) IS A(ROUTINE)                      
*        X'4000'                   ACCEPT =,/=                                  
*        X'2000'                   ACCEPT <,>,<=,=>                             
*        X'1000'                   HEX VALUE                                    
*        X'0800'                   DEC VALUE                                    
*        X'0400'                   OUTPUT IS A LIST                             
*        X'0200'                   TIME VALUE                                   
*                                                                               
         SPACE 1                                                                
CARDTAB  DS    0F                                                               
         DC    C'DDSIO  ',AL1(4,8),X'0000',AL3(DDSIO)                           
         DC    C'BOOK   ',AL1(3,10),X'0000',AL3(BOOK)                           
         DC    C'PHASET ',AL1(5,08),X'0400',AL3(PHASET)                         
         DC    C'PHASEL ',AL1(5,08),X'0400',AL3(PHASEL)                         
         DC    X'0000'          EXTEND                                          
         SPACE 1                                                                
VALCARD  NTR1                                                                   
         ST    RD,CARDRD                                                        
         LR    R2,R1                                                            
         LA    R1,79(R1)                                                        
         ST    R1,CARDEND          SAVE LAST CHR ADDR                           
         CLI   0(R2),C'*'          * IN COL 1 IS A COMMENT                      
         BE    EXITEQU                                                          
*                                                                               
VALC001  LA    R4,CARDTAB                                                       
         ST    R2,CARDR2                                                        
VALC010  SR    R1,R1               GET LEN FOR COMPARE                          
         IC    R1,7(R4)                                                         
         EX    R1,*+8              EXECUTE KEYWORD TEST                         
         B     *+10                                                             
         CLC   0(0,R2),0(R4)                                                    
         BE    VALC020                                                          
         LA    R4,14(R4)           TRY NEXT ENTRY                               
         CLI   0(R4),0                                                          
         BNE   VALC010                                                          
         B     CERRKEY             ERROR INVALID KEYWORD                        
*                                                                               
VALC020  LA    R2,1(R2,R1)         POINT TO DELIMITER                           
*                                                                               
         LA    RF,VALCDELS         DELIMITER TABLE                              
         B     *+8                                                              
VALC021  LA    RF,5(RF)                                                         
         CLI   0(RF),0                                                          
         BE    CERRDEL             END OF TABLE INVALID DELIMITER               
*                                                                               
         MVC   BYTE,4(RF)          AUTH BIT MUST BE ON                          
         CLI   BYTE,0              EXCEPT WHEN ZERO                             
         BE    *+14                                                             
         NC    BYTE,9(R4)                                                       
         BZ    VALC021                                                          
*                                                                               
         SR    R1,R1                                                            
         IC    R1,2(RF)            GET EX LEN                                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),0(RF)       TEST DELIMITERS                              
         BNE   VALC021                                                          
*                                                                               
         MVC   BYTE,3(RF)          SAVE COMPARE CHR                             
         LA    R2,1(R1,R2)                                                      
         B     VALC025                                                          
*                                                                               
VALCDELS DC    C'= ',AL1(0),X'80',X'00'                                         
         DC    C'>=',AL1(1),X'B0',X'20'                                         
         DC    C'<=',AL1(1),X'D0',X'20'                                         
         DC    C'/=',AL1(1),X'70',X'40'                                         
         DC    C'< ',AL1(0),X'40',X'20'                                         
         DC    C'> ',AL1(0),X'20',X'20'                                         
         DC    X'00'                                                            
*                                                                               
VALC025  LR    R1,R2               GET LEN FOR MOVE                             
VALC026  CLI   0(R1),C','                                                       
         BE    VALC030                                                          
         CLI   0(R1),C' '                                                       
         BE    VALC030                                                          
         CLI   0(R1),0                                                          
         BE    VALC030                                                          
         LA    R1,1(R1)                                                         
         B     VALC026                                                          
*                                                                               
VALC030  SR    R1,R2                                                            
*                                                                               
VALC031  BCTR  R1,0                                                             
         SR    RF,RF                                                            
         ICM   RF,7,11(R4)         GET ADDRESS FOR MOVE                         
*                                                                               
         TM    9(R4),X'80'         IF ROUTINE                                   
         BZ    *+10                                                             
         BASR  RE,RF               GOTO ROUTINE                                 
         B     VALC090                                                          
*                                                                               
         TM    9(R4),X'04'         IF LIST                                      
         BNO   VALC050                                                          
VALC040  CLI   0(RF),X'FF'         CHECK NOT FULL                               
         BE    CERRMAN                                                          
         CLI   0(RF),C' '          EMPTY ENTRY                                  
         BNH   VALC050                                                          
         SR    R0,R0                                                            
         IC    R0,8(R4)                                                         
         AR    RF,R0                                                            
         TM    9(R4),X'60'         /<=>                                         
         BZ    VALC040                                                          
         LA    RF,1(RF)            ONE MORE FOR CODE                            
         B     VALC040                                                          
*                                                                               
VALC050  TM    9(R4),X'60'         IF /<=>                                      
         BZ    *+14                                                             
         MVC   0(1,RF),BYTE        SAVE COMP CODE                               
         LA    RF,1(RF)                                                         
*                                                                               
         TM    9(R4),X'10'         HEX INPUT                                    
         BNO   VALC060                                                          
         LA    R0,1(R1)            SET R0 HEX INPUT LEN                         
         GOTO1 =V(HEXIN),DMCB,(R2),(RF),(R0)                                    
         ICM   R1,15,12(R1)                                                     
         BZ    CERRHEX                                                          
         B     VALC090                                                          
*                                                                               
VALC060  TM    9(R4),X'08'         DEC INPUT                                    
         BZ    VALC070                                                          
         LR    R4,R2                                                            
         LA    R3,1(R1)                                                         
         BAS   RE,VALNUM           VALIDATE NUMBER                              
         CLI   DUB,X'FF'                                                        
         BE    CERRDEC                                                          
         CVB   R1,DUB                                                           
         STH   R1,0(RF)            SAVE HALFWORD (DEFAULT)                      
         B     VALC090                                                          
*                                                                               
VALC070  TM    9(R4),X'02'         TIME INPUT                                   
         BZ    VALC080                                                          
         BAS   RE,VALTIME                                                       
         MVC   0(4,RF),FULL                                                     
         B     VALC090                                                          
*                                                                               
VALC080  CLI   8(R4),0             DONT CARE                                    
         BE    *+12                                                             
         CLM   R1,1,8(R4)          CHECK MAX LEN                                
         BNL   CERRMAX                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),0(R2)       MOVE TO OUTPUT AREA                          
*                                                                               
VALC090  CLI   0(R2),C','          TEST FOR ANOTHER                             
         LA    R2,1(R2)                                                         
         BE    VALC001             GO FIND TABLE ENTRY                          
         C     R2,CARDEND          TEST FOR END OF CARD                         
         BL    VALC090                                                          
*                                                                               
EXITEQU  CR    RB,RB               SET CC EQU                                   
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
CERRSYS  LA    R1,=C'INVALID SYSTEM  '                                          
         B     CERRX                                                            
CERRPRG  LA    R1,=C'INVALID PROGRAM '                                          
         B     CERRX                                                            
CERRSES  LA    R1,=C'INVALID SESYS   '                                          
         B     CERRX                                                            
CERRTIM  LA    R1,=C'INVALID TIME    '                                          
         B     CERRX                                                            
CERRDEC  LA    R1,=C'MUST BE HEX     '                                          
         B     CERRX                                                            
CERRHEX  LA    R1,=C'MUST BE DECIMAL '                                          
         B     CERRX                                                            
CERRKEY  LA    R1,=C'INVALID KEYWORD '                                          
         B     CERRX                                                            
CERRDEL  LA    R1,=C'INVALID DELIMITR'                                          
         B     CERRX                                                            
CERRMAX  LA    R1,=C'VALUE TOO LONG  '                                          
         B     CERRX                                                            
CERRMAN  LA    R1,=C'TOO MANY FILTERS'                                          
         B     CERRX                                                            
*                                                                               
CERRX    L     RD,CARDRD                                                        
         L     R2,CARDR2                                                        
         LA    RF,PLINE+1                                                       
CERRX1   MVC   0(1,RF),0(R2)                                                    
         CLI   0(RF),C' '                                                       
         BE    CERRX2                                                           
         CLI   0(RF),C','                                                       
         BE    CERRX2                                                           
         LA    R2,1(R2)                                                         
         LA    RF,1(RF)                                                         
         B     CERRX1                                                           
*                                                                               
CERRX2   LA    RF,1(RF)                                                         
         MVC   0(13,RF),=C'*** ERROR ***'                                       
         LA    RF,14(RF)                                                        
         MVC   0(16,RF),0(R1)                                                   
         BAS   RE,PRINTL                                                        
*                                                                               
EXITNEQ  LTR   RB,RB               SET CC NEQ                                   
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDVALNUM                                                       
*************************************************************                   
*        GET TIME FROM 0(R2) (R1)=EX LEN  TIME=HH:MM:SS.TU  *                   
*************************************************************                   
         SPACE 1                                                                
VALTIME  NTR1                                                                   
         MVC   HALF,=C'00'         FIRST MAY BE 1:00 OR 02:00                   
         CLI   1(R2),C':'                                                       
         BNE   VALT010                                                          
*                                                                               
         MVC   HALF+1(1),0(R2)     ASSUME 1:00                                  
         LA    R2,2(R2)                                                         
         B     VALT020                                                          
*                                                                               
VALT010  MVC   HALF+0(2),0(R2)     ASSUME 02:00                                 
         LA    R2,3(R2)                                                         
*                                                                               
VALT020  LA    R3,2                PREPARE FULL AND HALD                        
         LA    R4,HALF                                                          
         XC    FULL,FULL                                                        
*                                                                               
         BAS   RE,VALNUM           VALIDATE HOURS                               
         L     RF,=A(60*60*100)                                                 
         BAS   RE,VALTADD                                                       
*                                                                               
         MVC   HALF,0(R2)          VALIDATE MINUTES                             
         BAS   RE,VALNUM                                                        
         L     RF,=A(60*100)                                                    
         BAS   RE,VALTADD                                                       
*                                                                               
         CLI   2(R2),C':'          TEST FOR SECS                                
         BNE   EXITEQU                                                          
         LA    R2,3(R2)                                                         
         MVC   HALF,0(R2)                                                       
         BAS   RE,VALNUM           VALIDATE SECS                                
         L     RF,=F'100'                                                       
         BAS   RE,VALTADD                                                       
*                                                                               
         CLI   2(R2),C'.'          TEST FOR TUS                                 
         BNE   EXITEQU                                                          
         LA    R2,3(R2)                                                         
         MVC   HALF,0(R2)                                                       
         BAS   RE,VALNUM           VALIDATE TUS                                 
         LA    RF,1                                                             
         BAS   RE,VALTADD                                                       
         B     EXITEQU                                                          
*                                                                               
VALTADD  CLI   DUB,X'FF'           TEST FOR INVALID NUMERIC                     
         BE    CERRTIM                                                          
         SR    R0,R0               CONVERT AND MULTIPLY BY RF                   
         CVB   R1,DUB                                                           
         MR    R0,RF                                                            
         A     R1,FULL                                                          
         ST    R1,FULL             ADD TO FULL                                  
         BR    RE                                                               
         EJECT                                                                  
*************************************************************                   
*        PRINT ROUTINES                                     *                   
*************************************************************                   
         SPACE 1                                                                
PRINTI   ST    RE,SAVERE                                                        
         OPEN  (SYSPRINT,OUTPUT)   PRINT INIT                                   
         ZAP   LINE,=P'0'                                                       
         ZAP   PAGE,=P'1'                                                       
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
PRINTT   ST    RE,SAVERE           PRINT TITLES                                 
         ZAP   LINE,=P'0'          RESET LINECOUNT                              
         AP    PAGE,=P'1'          BUMP PAGECOUNT                               
         PUT   SYSPRINT,STARS      PRINT TITLE                                  
         PUT   SYSPRINT,TITLE      PRINT TITLE                                  
         PUT   SYSPRINT,STARS2     PRINT TITLE                                  
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
PRINTB   MVC   PLINE,SPACES                                                     
*                                                                               
PRINTL   ST    RE,SAVERE           PRINT LINE                                   
         AP    LINE,=P'1'          BUMP LINECOUNT                               
         CP    LINE,MAXLINE        TEST FOR MAX LINES                           
         BL    PRINTL2                                                          
*                                                                               
PRINTL1  ZAP   LINE,=P'1'          RESET LINECOUNT                              
         AP    PAGE,=P'1'          BUMP PAGECOUNT                               
         PUT   SYSPRINT,STARS      PRINT TITLE                                  
         PUT   SYSPRINT,TITLE      PRINT TITLE                                  
         PUT   SYSPRINT,STARS2     PRINT TITLE                                  
*                                                                               
PRINTL2  PUT   SYSPRINT,PLINE      PRINT LINE                                   
         MVC   PLINE,SPACES                                                     
         L     RE,SAVERE                                                        
         BR    RE                  EXIT                                         
*                                                                               
PRINTX   ST    RE,SAVERE           CLOSE PRINT                                  
         CLOSE SYSPRINT                                                         
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*************************************************************                   
*        PRINT TITLE                                        *                   
*************************************************************                   
         SPACE 1                                                                
STARS    DC    C'i'                                                             
         DC    165C'*'                                                          
STARS2   DC    C' '                                                             
         DC    165C'*'                                                          
TITLE    DC    CL166' '                                                         
TITLE1   DC    C' BOOKS WITH PHASE CARDS                           '            
TITLE2   DC    C' UNCOMPILED SOURCE BOOKS                          '            
TITLE3   DC    C' ORPHANED RELO MODULES                            '            
TITLE4   DC    C' BOOKS CORRECTLY COMPILED AND IN SYNC UK/US       '            
TITLE5   DC    C' BOOKS CORRECTLY COMPILED UK ONLY                 '            
TITLE6   DC    C' BOOKS CORRECTLY COMPILED US ONLY                 '            
TITLE7   DC    C' THE REST                                         '            
         ORG                                                                    
         EJECT                                                                  
*************************************************************                   
*        DCBS                                               *                   
*************************************************************                   
*                                                                               
*        LRECL=(166) USE CHARS=(BX15)                                           
*                                                                               
SYSPRINT DCB   DSORG=PS,MACRF=PM,DDNAME=SYSPRINT,RECFM=FBA,LRECL=(166)          
         EJECT                                                                  
*************************************************************                   
*        CONSTANTS & LTORG                                  *                   
*************************************************************                   
         SPACE 1                                                                
MAXLINE  DC    P'60'                                                            
SPACES   DC    CL166' '                                                         
DOTS     DC    64C'.'                                                           
FFS      DC    64X'FF'                                                          
         SPACE 1                                                                
DMOPEN   DC    CL8'DMOPEN'                                                      
DMCLSE   DC    CL8'DMCLSE'                                                      
DMUNLK   DC    CL8'DMUNLK'                                                      
DMREAD   DC    CL8'DMREAD'                                                      
DMRDHI   DC    CL8'DMRDHI'                                                      
DMRSEQ   DC    CL8'DMRSEQ'                                                      
DMWRT    DC    CL8'DMWRT'                                                       
ADDREC   DC    CL8'ADDREC'                                                      
GETREC   DC    CL8'GETREC'                                                      
PUTREC   DC    CL8'PUTREC'                                                      
*                                                                               
PERSON   DC    CL8'PERSON'                                                      
MEDIA    DC    CL8'MEDIA '                                                      
PERDIR   DC    CL8'PERDIR'                                                      
PERFIL   DC    CL8'PERFIL'                                                      
*                                                                               
PFILES   DC    CL8'NPERDIR '                                                    
         DC    CL8'NPERFIL '                                                    
         DC    CL8'NPERREQ '                                                    
         DC    CL8'NPERRCV '                                                    
         DC    CL8'X       '                                                    
*                                                                               
         LTORG                                                                  
*                                                                               
KEYBUFF  DS    2000CL36                                                         
KEYBUFFX EQU   *                                                                
*                                                                               
         EJECT                                                                  
*************************************************************                   
*        SSB                                                *                   
*************************************************************                   
         SPACE 1                                                                
SSB      CSECT                                                                  
         DC    256X'00'                                                         
         ORG   SSB                                                              
         DC    XL2'00',X'FF',X'00'    RECOVERY                                  
         DC    5XL4'00000000'                                                   
         DC    A(0)                                                             
         DC    A(0)                                                             
         ORG   SSB+45                                                           
         DC    C'P'                   NO DSPACE "GET FROM PERFIL"               
         ORG   SSB+52                                                           
         DC    A(SSB)                                                           
         ORG                                                                    
*************************************************************                   
*        WORKING STORAGE                                    *                   
*************************************************************                   
         SPACE 1                                                                
WORKC    CSECT                                                                  
         DS    (64*1024)X             WORKING STORAGE POOL                      
         EJECT                                                                  
*************************************************************                   
*        DSECT TO COVER WORKING STORAGE                     *                   
*************************************************************                   
         SPACE 1                                                                
WORKD    DSECT                                                                  
SAVERD   DS    A                                                                
SAVERE   DS    A                                                                
CARDRD   DS    A                                                                
CARDR2   DS    A                                                                
CARDEND  DS    A                                                                
*                                                                               
DUB      DS    D                                                                
DUB1     DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
FLAG1    DS    X                                                                
*                                                                               
FLAGWORD DS    CL4                                                              
*                                                                               
DMCB     DS    6F                                                               
WORK     DS    CL255                                                            
WORK1    DS    XL64                                                             
IOW      DS    12D                                                              
DA       DS    F                                                                
*                                                                               
KEYFLAGS DS    0CL4                                                             
KEYFLAG1 DS    C                                                                
KEYFLAG2 DS    C                                                                
KEYFLAG3 DS    C                                                                
KEYFLAG4 DS    C                                                                
*                                                                               
AIO1     DS    A                                                                
*                                                                               
PERKEY   DS    CL42                                                             
*                                                                               
KEYSAVE  DS    CL36                                                             
*                                                                               
LINE     DS    PL2                                                              
PAGE     DS    PL4                                                              
PLINE    DS    CL166                                                            
*                                                                               
INCLAREA DS    500CL8                                                           
SORCAREA DS    500CL10                                                          
*                                                                               
BUFF2    DS    CL14336                                                          
         ORG   BUFF2                                                            
IOAREA   DS    4096C                                                            
IOAREA1  DS    4096C                                                            
         ORG                                                                    
*                                                                               
WORKX    EQU   *                                                                
         EJECT                                                                  
************************************************************                    
*        OTHER DSECTS                                      *                    
************************************************************                    
         SPACE 1                                                                
PLINED   DSECT                                                                  
         DS    CL1                                                              
PLIBOOK  DS    CL10                                                             
         DS    CL1                                                              
PLILEV   DS    CL3                                                              
         DS    CL1                                                              
PLIDATE  DS    CL8                                                              
         DS    CL1                                                              
PLITYPE  DS    CL8                                                              
         DS    CL1                                                              
PLICTRY  DS    CL2                                                              
         DS    CL1                                                              
PLILIB   DS    CL8                                                              
         DS    CL1                                                              
PLINAME  DS    CL8                                                              
         DS    CL1                                                              
PLINAME2 DS    CL8                                                              
         SPACE 1                                                                
*FASSBOFF                                                                       
       ++INCLUDE FASSBOFF                                                       
*PEGENPAN                                                                       
       ++INCLUDE PEGENPAN                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'209PELEVLM   07/17/07'                                      
         END                                                                    
******************>?????******************                                      
LMFACADV  111 10/JAN/01                                                         
******************>?????******************                                      
*PHASE TE0A00A                                                                  
*&&      SET   NOP=N                                                            
         PRINT NOGEN                                                            
PELEVX   CSECT                                                                  
         NMODL WORKX-WORKD,**LEVX*,RA,RR=R4,CLEAR=YES                           
         USING WORKD,RC                                                         
         ST    R4,RELO                                                          
         ST    RD,SAVERD                                                        
         MVC   SVPARMS,0(R1)                                                    
         L     R8,ACOMFACS                                                      
         USING COMFACSD,R8         R8=A(COMFACS)                                
         L     R9,ATWA                                                          
         USING PELEVFFD,R9         R9=A(TWA)                                    
         USING TWAD,TWAHDR                                                      
         LA    R7,3072(R9)         R7=A(TWA SAVE AREA)                          
         USING SAVED,R7                                                         
*                                                                               
         USING PEPANLD,PERKEY      SET UP KEY USING                             
*                                                                               
         L     RF,=A(IOAREA-WORKD)                                              
         LA    RF,WORKD(RF)                                                     
         ST    RF,AIOAREA                                                       
*                                                                               
         L     RF,ATIOB            SET CURSOR INF FROM TIOB                     
         USING TIOBD,RF                                                         
         SR    R0,R0                                                            
         ICM   R0,3,TIOBCURD                                                    
         AR    R0,R9               ADD TWA ADDRESS                              
         ST    R0,CURSOR                                                        
         MVC   PFKEY,TIOBAID       SAVE PFKEY DATA                              
         SR    R0,R0                                                            
         IC    R0,PFKEY                                                         
         CLI   PFKEY,12            TEST FOR PF 13 - 24                          
         BNH   *+8                                                              
         SH    R0,=H'12'           CONVERT TO 7 - 12                            
         STC   R0,PFKEY                                                         
*                                                                               
         BAS   RE,VALIDATE                                                      
         CLI   CHANGED,C'Y'        IF REQUEST HAS CHANGED                       
         BE    MAIN020                                                          
*                                                                               
*AIN015  EQU   *     BAS   RE,VALSEL     SEE IF ANYTHING SELECTED               
*        BNE   MAIN030                                                          
*        XC    DISPPAGE,DISPPAGE                                                
         B     MAIN040                                                          
*                                                                               
MAIN020  MVI   MODE,C'L'                                                        
         SR    R1,R1               CLEAR SAVED STORAGE                          
         SR    R0,R0                                                            
         LH    RF,=Y(SAVECLRL)                                                  
         LA    RE,SAVECLR                                                       
         MVCL  RE,R0                                                            
*                                                                               
MAIN040  BAS   RE,LIST             LIST LEVEL INFO                              
         B     EXITX                                                            
*                                                                               
EXITX    L     R1,CURSOR           SET CURSOR FLAG                              
         OI    6(R1),X'40'                                                      
         XMOD1                                                                  
         SPACE 1                                                                
*                                                                               
XITNO    LTR   RB,RB                                                            
         B     XIT1                                                             
XITYES   CR    RB,RB                                                            
*                                                                               
XIT1     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE INPUT FIELD                                                *         
***********************************************************************         
         SPACE 1                                                                
VALIDATE NTR1                                                                   
         MVC   OLDBOOK,NOWBOOK     PRESET SCAN FIELDS                           
         MVC   OLDLIB,NOWLIB                                                    
         MVC   OLDOPT,NOWOPT                                                    
         XC    NOWOPT,NOWOPT                                                    
         XC    NOWBOOK,NOWBOOK                                                  
         XC    NOWLIB,NOWLIB                                                    
         XC    BOOKLEN,BOOKLEN                                                  
*                                                                               
VALID010 SR    R1,R1               ANY LIBRARY FILTER                           
         ICM   R1,1,LEVLIBH+5                                                   
         BZ    VALID020                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8              MOVE LIB TO FIELD                            
         B     *+10                                                             
         MVC   NOWLIB(0),LEVLIB                                                 
         STC   R1,LIBLEN                                                        
         OI    LEVLIBH+6,X'80'     XMIT                                         
*                                                                               
VALID020 SR    R1,R1               ANY BOOK REQUESTED                           
         ICM   R1,1,LEVBOOH+5                                                   
         BZ    VALID025                                                         
         STC   R1,BOOKLEN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8              MOVE BOOKNAME TO FIELD                       
         B     *+10                                                             
         MVC   NOWBOOK(0),LEVBOO                                                
         OI    LEVBOOH+6,X'80'     XMIT                                         
*                                                                               
VALID025 SR    R1,R1               ANY OPTIONS                                  
         ICM   R1,1,LEVOPTH+5                                                   
         BZ    VALID030                                                         
         MVC   NOWOPT,LEVOPT                                                    
         OI    LEVOPTH+6,X'80'     XMIT                                         
*                                                                               
VALID030 CLC   OLDBOOK,NOWBOOK     HAS REQUEST CHANGED                          
         BNE   VALID050                                                         
         CLC   OLDLIB,NOWLIB                                                    
         BNE   VALID050                                                         
         CLC   OLDOPT,NOWOPT                                                    
         BNE   VALID050                                                         
*                                                                               
VALID040 MVI   CHANGED,C'N'        NO CHANGE                                    
         B     XITYES                                                           
*                                                                               
VALID050 MVI   CHANGED,C'Y'        YES CHANGED                                  
         B     XITYES                                                           
         EJECT                                                                  
***********************************************************************         
* LIST LEVEL INFO                                                     *         
***********************************************************************         
         SPACE 1                                                                
LIST     NTR1                                                                   
         MVC   LEVPFK,SPACES                                                    
         MVC   LEVPFK,LISTPFK                                                   
         OI    LEVPFKH+6,X'80'                                                  
*                                                                               
         MVI   PELPANID,PELPANIQ                                                
         MVC   PELBOOK,NOWBOOK                                                  
*                                                                               
         OC    KEYLAST,KEYLAST     ANY PREVIOUS KEY                             
         BZ    LIST010                                                          
         MVC   PERKEY,KEYLAST                                                   
*                                                                               
LIST005  CLI   PFKEY,8             8 IS SAME AS ENTER                           
         BE    LIST010                                                          
         CLI   PFKEY,7             7 BACK UP ONE PAGE                           
         BNE   LIST010                                                          
*                                                                               
         ICM   RF,15,LISTPAGE      BACK UP ONE PAGE                             
         CHI   RF,1                                                             
         BL    INFO2                                                            
         BNE   *+8                                                              
         LA    RF,2                                                             
         SHI   RF,2                                                             
*                                                                               
         STCM  RF,15,LISTPAGE                                                   
         MHI   RF,36                                                            
         LA    RF,KEYLIST(RF)                                                   
         MVC   PERKEY,0(RF)                                                     
*                                                                               
LIST010  L     RF,LISTPAGE         SAVE TOP KEY FOR THIS PAGE                   
         MHI   RF,36                                                            
         LA    RF,KEYLIST(RF)                                                   
         MVC   0(36,RF),PERKEY                                                  
         LA    RF,1                                                             
         A     RF,LISTPAGE                                                      
         ST    RF,LISTPAGE                                                      
         GOTO1 CDATAMGR,DMCB,DMRDHI,PERDIR,PERKEY,PERKEY                        
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R4,LEVSELH                                                       
*                                                                               
LIST015  OI    6(R4),X'80'         ZAP ALL FIELDS TO SPACES                     
         MVC   8(2,R4),SPACES                                                   
         LA    R4,10(R4)                                                        
         OI    6(R4),X'80'                                                      
         MVC   8(75,R4),SPACES                                                  
         LA    R4,83(R4)                                                        
         LA    R1,LEVPFKH          NEXT                                         
         CR    R4,R1                                                            
         BL    LIST015                                                          
*                                                                               
         LA    R4,LEVSELH                                                       
         USING LSTLINED,R4                                                      
*                                                                               
         BAS   RE,CHKBK                                                         
         BNE   LIST991                                                          
*                                                                               
         BAS   RE,CHKLIB                                                        
         BE    LIST050                                                          
*                                                                               
LIST040  GOTO1 CDATAMGR,DMCB,DMRSEQ,PERDIR,PERKEY,PERKEY                        
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BAS   RE,CHKBK                                                         
         BNE   LIST991                                                          
*                                                                               
         BAS   RE,CHKLIB                                                        
         BNE   LIST040                                                          
*                                                                               
         CLC   BOOKLEVL,PELBOOK    SAME BOOK/LEVEL/DATE                         
         BE    LIST050                                                          
*                                                                               
         MVC   LSTLIN,DASHES                                                    
*                                                                               
         LA    R4,8+2+8+75(R4)                                                  
         LA    R1,LEVPFKH          NEXT                                         
         CR    R4,R1                                                            
         BNL   LIST990                                                          
*                                                                               
LIST050  MVC   BOOKLEVL,PELBOOK                                                 
         MVC   LSTBOOK,PELBOOK                                                  
         EDIT  (B1,PELLEV),(3,LSTLEV),FILL=0                                    
         MVC   HALF,PELDAT                                                      
         XC    HALF,=X'FFFF'                                                    
         GOTO1 CDATCON,DMCB,(2,HALF),(11,LSTDATE)                               
*                                                                               
         CLI   PELTYP,C'A'                                                      
         BE    *+12                                                             
         CLI   PELTYP,C'B'                                                      
         BNE   LIST060                                                          
         MVC   LSTLIB(4),=C'PAN.'                                               
         MVC   LSTLIB+4(4),PELLIB                                               
         MVC   LSTNAME,=C'**SOURCE**'                                           
*                                                                               
         GOTO1 CDATAMGR,DMCB,GETREC,PERFIL,PELDA,AIOAREA,IOWORK                 
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA                                                       
         LA    R2,PELDATA-PEPANLD(R2)                                           
         LA    R1,INCLAREA                                                      
LIST051  SR    R0,R0                                                            
         CLI   0(R2),0                                                          
         BE    LIST052                                                          
         CLI   0(R2),C'I'                                                       
         BNE   *+14                                                             
         MVC   0(8,R1),2(R2)                                                    
         LA    R1,8(R1)                                                         
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     LIST051                                                          
*                                                                               
LIST052  XC    0(8,R1),0(R1)                                                    
*                                                                               
LIST060  CLI   PELTYP,C'C'                                                      
         BNE   LIST062                                                          
         MVC   LSTLIB(4),=C'PAN.'                                               
         MVC   LSTLIB+4(4),PELLIB                                               
         MVC   LSTNAME(2),=C'RM'                                                
         MVC   LSTNAME+2(8),PELTEST                                             
*                                                                               
LIST062  CLI   PELTYP,C'L'                                                      
         BNE   LIST063                                                          
         MVC   LSTLIB(4),PELLIB                                                 
         MVC   LSTLIB+4(3),=C'LIB'                                              
         MVC   LSTNAME(8),PELTEST                                               
         CLC   PELTEST,SPACES                                                   
         BNE   *+10                                                             
         MVC   LSTNAME(8),PELPROD                                               
*                                                                               
LIST063  CLI   PELTYP,C'P'                                                      
         BNE   LIST065                                                          
         MVC   LSTLIB(4),=C'PROG'                                               
         MVC   BYTE,PELLIB+3                                                    
         MVC   HALF+0(1),PELCTRY                                                
         MVC   HALF+1(1),PELLIB+3                                               
         BAS   RE,GETADV                                                        
         MVC   LSTLIB+4(4),FULL                                                 
         MVC   LSTNAME(8),PELTEST                                               
         CLC   PELTEST,SPACES                                                   
         BNE   *+10                                                             
         MVC   LSTNAME(8),PELPROD                                               
*                                                                               
LIST065  MVC   LSTCTRY,=C'UK'                                                   
         CLI   PELCTRY,C'S'                                                     
         BNE   *+10                                                             
         MVC   LSTCTRY,=C'US'                                                   
*                                                                               
         CLI   PELTYP,C'C'                                                      
         BL    LIST090                                                          
*                                                                               
LIST070  LR    R1,R4               BACK UP R1 TO PREVIOUS LINE                  
         SHI   R1,8+2+8+75                                                      
O        USING LSTLINED,R1                                                      
         CLC   O.LSTBOOK(LSTNAME-LSTBOOK),LSTBOOK                               
         BNE   LIST090                                                          
         LA    RE,LSTNAME                                                       
         LA    RF,O.LSTNAME                                                     
         LA    R0,10                                                            
LIST071  CLC   0(1,RE),0(RF)       COMPARE PHASE NAMES                          
         BNE   LIST072                                                          
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,LIST071                                                       
         B     LIST072A                                                         
*                                                                               
LIST072  CLI   0(RF),C' '          WERE THEY SAME UP TO L'OLD                   
         BE    *+12                                                             
         CLI   1(RF),C' '          WERE WE ON LAST CHR                          
         BNE   LIST090                                                          
*                                                                               
LIST072A LA    R1,O.LSTNAME        COPY THIS PHASE TO PREVIOUS LINE             
         LA    R0,3                                                             
LIST073  CLI   0(R1),C' '                                                       
         BE    LIST074                                                          
         LA    R1,11(R1)           NEXT PHASE                                   
         BCT   R0,LIST073                                                       
         B     LIST090             UNLESS NO ROOM                               
         DROP  O                                                                
*                                                                               
LIST074  MVC   0(10,R1),LSTNAME    COPY IN PHASE                                
         MVC   LSTLIN,SPACES       CLEAR THIS LINE                              
         B     LIST040                                                          
*                                                                               
LIST090  LA    R4,8+2+8+75(R4)                                                  
         LA    R1,LEVPFKH          NEXT                                         
         CR    R4,R1                                                            
         BL    LIST040                                                          
*                                                                               
LIST990  MVC   KEYLAST,PERKEY                                                   
*                                                                               
LIST991  EQU   *                                                                
*                                                                               
LIST999  B     INFO2                                                            
*                                                                               
CHKBK    SR    R1,R1                                                            
         ICM   R1,1,BOOKLEN                                                     
         BZR   RE                                                               
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   NOWBOOK(0),PELBOOK                                               
         BNER  RE                                                               
         BR    RE                                                               
*                                                                               
CHKLIB   SR    R1,R1                                                            
         ICM   R1,1,LIBLEN                                                      
         BZR   RE                                                               
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   NOWLIB(0),PELLIB                                                 
         BNER  RE                                                               
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
* GET SYSTEM NAME FROM GTADV                                          *         
***********************************************************************         
         SPACE 1                                                                
GETADV   NTR1                                                                   
         LA    RE,PGMSTAB                                                       
*                                                                               
GADV02   CLC   HALF,0(RE)          EOT                                          
         BE    GADV03                                                           
         LA    RE,6(RE)                                                         
         CLC   0(2,RE),=C'**'                                                   
         BNE   GADV02                                                           
*                                                                               
GADV03   MVC   FULL,2(RE)          GET NAME                                     
         B     XITYES                                                           
*                                                                               
PGMSTAB  DC    C'KF',C'LCSC'                                                    
         DC    C'KN',C'NEW '                                                    
         DC    C'KS',C'TTS '                                                    
         DC    C'KT',C'LTST'                                                    
         DC    C'KQ',C'LFQA'                                                    
         DC    C'KA',C'LADV'                                                    
         DC    C'S2',C'MEL '                                                    
         DC    C'SA',C'NADV'                                                    
         DC    C'SC',C'NCSC'                                                    
         DC    C'SQ',C'NFQA'                                                    
         DC    C'SR',C'REP '                                                    
         DC    C'ST',C'NTST'                                                    
         DC    C'**',C'????'                                                    
         EJECT                                                                  
***********************************************************************         
* GET SYSTEM NAME FROM GTADV                                          *         
***********************************************************************         
*        SPACE 1                                                                
*ETADV   NTR1                                                                   
*        LA    RE,FACIDTAB                                                      
*        USING FACITABD,RE                                                      
*        LHI   RF,L'FACITAB                                                     
*                                                                               
*ADV02   CLI   FACITAB,255         EOT                                          
*        BE    XITNO                                                            
*        CLC   BYTE,FACISN1        MATCH ADV CHR                                
*        BE    *+8                                                              
*        BXH   RE,RF,GADV02                                                     
*                                                                               
*        MVC   FULL,FACISN4        GET NAME                                     
*        B     XITYES                                                           
*        DROP  RE                                                               
*        EJECT                                                                  
***********************************************************************         
* ERROR EXITS                                                         *         
***********************************************************************         
         SPACE 1                                                                
ERR1     MVC   LEVHDR(25),=C'No matches found         '                         
         B     ERRX                                                             
ERR2     MVC   LEVHDR(25),=C'No more books in list    '                         
         B     ERRX                                                             
ERR3     MVC   LEVHDR(25),=C'This is the first book   '                         
         B     ERRX                                                             
ERR4     MVC   LEVHDR(25),=C'Error - No report created'                         
         B     ERRX                                                             
ERR5     MVC   LEVHDR(25),=C'Error - Loading tempest  '                         
         B     ERRX                                                             
ERR6     MVC   LEVHDR(25),=C'Test systems only please '                         
         B     ERRX                                                             
INFO1    MVC   LEVHDR(25),=C'Enter field to search for'                         
         B     ERRX                                                             
INFO2    MVC   LEVHDR(25),=C'Book details displayed   '                         
         B     ERRX                                                             
INFO3    MVC   LEVHDR(25),=C'Report PAN,              '                         
         EDIT  (B2,HALF),(5,LEVHDR+11),ALIGN=LEFT                               
         LA    R1,LEVHDR+12                                                     
         AR    R1,R0                                                            
         MVC   0(10,R1),=C'Spooled '                                            
         B     ERRX                                                             
*                                                                               
ERRX     OI    LEVHDRH+6,X'80'                                                  
         L     RD,SAVERD                                                        
         B     EXITX                                                            
         EJECT                                                                  
***********************************************************************         
* CONSTANTS AND LITERALS                                              *         
***********************************************************************         
         SPACE 1                                                                
DMOPEN   DC    CL8'OPEN   '                                                     
DMPRINT  DC    CL8'DMPRINT'                                                     
DMCLOSE  DC    CL8'CLOSE  '                                                     
CLOSE    DC    CL8'CLOSE  '                                                     
PRTQUE   DC    CL8'PRTQUE'                                                      
DMRDHI   DC    C'DMRDHI '                                                       
DMRSEQ   DC    C'DMRSEQ '                                                       
GETREC   DC    C'GETREC '                                                       
PERDIR   DC    C'PERDIR '                                                       
PERFIL   DC    C'PERFIL '                                                       
READ     DC    C'READ   '                                                       
PAN      DC    C'PAN    '                                                       
*                                                                               
DASHES   DC    80C'-'                                                           
DASHES2  DC    40C'- '                                                          
SPACES   DC    CL80' '                                                          
LISTPFK  DC    CL40'PF7=Up PF8=Down                         '                   
         DC    CL38'                                      '                     
DISPPFK  DC    CL40'PF7=Up PF8=Down PF5=Previous Book PF6=Ne'                   
         DC    CL38'xt Book PF3=Return PF4=Print          '                     
BOOKL1   DC    C'- - - - - BOOK=           LEVEL     DATE'                      
         DC    C'=.../.. - - - - -                  '                           
HEADER1  DC    CL40'S      Source Object name  Load librarys'                   
         DC    CL38'   Program dataspaces                 '                     
HEADER2  DC    CL40'L  Cty Lib    and library  Loadlib Testl'                   
         DC    CL38'ib adv tst new tts fqa csc            '                     
         LTORG                                                                  
       ++INCLUDE FACIDTAB                                                       
         EJECT                        adv tst new tts fqa csc                   
***********************************************************************         
* DSECT TO COVER SAVED STORAGE IN TWA                                 *         
***********************************************************************         
         SPACE 1                                                                
SAVED    DSECT                                                                  
*                                                                               
NOWOPT   DS    CL8                 IGNORE BOOKS                                 
NOWBOOK  DS    CL10                BOOK FILTER                                  
BOOKLEN  DS    CL1                 LENGTH OF BOOK FILTER                        
NOWLIB   DS    CL12                LIB FILTER                                   
LIBLEN   DS    CL1                 LENGTH - 1                                   
*                                                                               
BOOK     DS    CL10                                                             
DA       DS    CL4                                                              
*                                                                               
PREVBOOK DS    CL10                                                             
*                                                                               
MODE     DS    C                                                                
*                                                                               
JTODAY   DS    PL4                                                              
LOADJULE DS    PL4                                                              
LOADDATE DS    CL10                                                             
*                                                                               
SAVECLR  DS    0C                                                               
SAVECLRL EQU   SAVECLRX-SAVECLR                                                 
*                                                                               
KEYLIST  DS    100CL36             TOP KEYS 100 PAGES WORTH                     
KEYLAST  DS    CL36                LAST KEY ON PAGE                             
LISTPAGE DS    F                   CURRENT PAGE FOR LIST                        
*                                                                               
SAVECLRX EQU   *                                                                
         SPACE 2                                                                
***********************************************************************         
*DSECT TO COVER WORKING STORAGE                                       *         
***********************************************************************         
         SPACE 1                                                                
WORKD    DSECT                                                                  
SAVERD   DS    A                                                                
SAVERE   DS    A                                                                
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
PFKEY    DS    X                                                                
RELO     DS    A                                                                
CURSOR   DS    A                                                                
ATSARBUF DS    A                                                                
*                                                                               
IOWORK   DS    XL64                                                             
*                                                                               
CHANGED  DS    C                                                                
CODE     DS    C                                                                
NOT      DS    C                                                                
FLUSH    DS    C                                                                
*                                                                               
BOOKLEVL DS    CL13                CURRENT BOOK LEVEL DATE                      
BOOKLINE DS    CL72                                                             
*                                                                               
AIOAREA  DS    A                                                                
VPANIC   DS    A                                                                
VTSAR    DS    A                                                                
*                                                                               
         DS    0A                                                               
SVPARMS  DS    0XL32                                                            
ATIOB    DS    A                                                                
ATWA     DS    A                                                                
ALOCSYS  DS    A                                                                
ATIA     DS    A                                                                
ACOMFACS DS    A                                                                
AXTRA    DS    A                                                                
         DS    A                                                                
         DS    A                                                                
*                                                                               
APAGEBRK DS    A                                                                
*                                                                               
PERKEY   DS    CL44                                                             
*                                                                               
OLDOPT   DS    CL8                 PREVIOUS FILTERS                             
OLDBOOK  DS    CL10                                                             
OLDLIB   DS    CL12                                                             
*                                                                               
DMCB     DS    6F                                                               
WORK     DS    XL1024                                                           
DMWORK   DS    12D                                                              
*                                                                               
NONHDR   DS    CL5                                                              
NONLINE  DS    CL80                                                             
*                                                                               
         DS    0D                                                               
WLINEL   DS    CL8                                                              
WLINE    DS    0CL255                                                           
WCTL     DS    C                                                                
WDAT     DS    CL254                                                            
*                                                                               
INCLADDR DS    A                                                                
INCLAREA DS    500CL8                                                           
*                                                                               
IOAREA   DS    6144C                                                            
*                                                                               
WORKX    EQU   *                                                                
         EJECT                                                                  
***********************************************************************         
* LIST LINES                                                          *         
***********************************************************************         
         SPACE 1                                                                
LSTLINED DSECT                                                                  
LSTSELH  DS    XL8                                                              
LSTSEL   DS    CL2                                                              
LSTLINH  DS    XL8                                                              
LSTLIN   DS    0CL75                                                            
LSTBOOK  DS    CL10                                                             
         DS    CL1                                                              
LSTLEV   DS    CL3                                                              
         DS    CL1                                                              
LSTDATE  DS    CL8                                                              
         DS    CL1                                                              
LSTCTRY  DS    CL2                                                              
         DS    CL1                                                              
LSTLIB   DS    CL8                                                              
         DS    CL1                                                              
LSTNAME  DS    CL10                                                             
LSTNAMEX DS    CL1                                                              
         DS    CL21                                                             
         SPACE 1                                                                
LSTL2    DSECT                                                                  
LST2SEL  DS    XL8                                                              
LST2SELH DS    CL2                                                              
LST2LINH DS    XL8                                                              
LSTSLIN  DS    0CL75                                                            
LST2CTY  DS    CL3                                                              
         DS    CL1                                                              
LST2SRC  DS    CL6                                                              
         DS    CL1                                                              
LST2OBJ  DS    CL13                                                             
         DS    CL1                                                              
LST2LOAD DS    CL8                                                              
         DS    CL1                                                              
LST2TEST DS    CL8                                                              
         DS    CL1                                                              
LST2DSPC DS    CL23                                                             
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER TWA                                                  *         
***********************************************************************         
         SPACE 1                                                                
PELEVFFD DSECT                                                                  
TWAHDR   DS    CL64                                                             
       ++INCLUDE PELEVFFD                                                       
         SPACE 1                                                                
         DS    CL128' '            SPACE FOR HILITE HEADERS                     
TWAEND   EQU   *                                                                
*DDCOMFACS                                                                      
*DDACCFACS                                                                      
*PEGENPAN                                                                       
*FATIOB                                                                         
*DMPRTQL                                                                        
*FATWA                                                                          
*FACIDTABD                                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDACCFACS                                                      
       ++INCLUDE PEGENPAN                                                       
       ++INCLUDE FATIOB                                                         
       ++INCLUDE DMPRTQL                                                        
       ++INCLUDE FATWA                                                          
       ++INCLUDE FACIDTABD                                                      
**PAN#1  CSECT                                                                  
         END                                                                    
