*          DATA SET CTSFM17    AT LEVEL 005 AS OF 05/01/02                      
*PHASE TA0A17A                                                                  
                                                                                
***********************************************************************         
* TITLE: TA0A17 - OFFICE LIST RECORD MAINT/LIST                       *         
* COMMENTS: MAINTAINS ADDS OFFICE LIST RECORDS                        *         
***********************************************************************         
                                                                                
         TITLE 'TA0A17 OFFICE LIST MAINTENANCE/LIST'                            
TA0A17   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,TA0A17**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
         MVI   ACTELOPT,C'Y'       CREATE ACTIVITY ELEMENT ON RECORDS           
         MVI   NLISTS,NLSTLNQ                                                   
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS       ONLINE LIST RECORDS                          
         BE    LR                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    LR                                                               
         CLI   MODE,RECDEL         DELETE A RECORD                              
         BE    DELREC                                                           
         CLI   MODE,RECREST        RESTORE A RECORD                             
         BE    RESREC                                                           
         CLI   MODE,XRECPUT        RECORD JUST REPLACED                         
         BE    XRR                                                              
         CLI   MODE,XRECADD        RECORD JUST ADDED                            
         BE    XRA                                                              
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* VALIDATE KEY ROUTINE                                               *          
**********************************************************************          
                                                                                
VK       XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING CTUREC,R6           BUILD DEFINITION RECORD KEY                  
         MVI   CTUKEY,C'U'                                                      
         MVC   CTUKPROG,SPACES                                                  
         MVI   CTUKLANG,0                                                       
         MVC   CTUKAGY,AGENCY      AGENCY                                       
*                                                                               
VK1      LA    R2,OFFSYSH          SYSTEM                                       
         SR    R1,R1                                                            
         ICM   R1,1,5(R2)                                                       
         BZ    MISSFLD                                                          
         BCTR  R1,0                                                             
         LA    RE,SYSTAB                                                        
VK2      CLI   0(RE),C' '          SEARCH LIST OF VALID SYSTEMS                 
         BE    INVLFLD                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),8(R2)                                                    
         BE    VK3                                                              
         LA    RE,L'SYSTAB(RE)                                                  
         B     VK2                                                              
VK3      MVC   CTUKSYS,7(RE)                                                    
*                                                                               
VK4      CLI   ACTNUM,ACTLIST      IF ACTION LIST OR REPORT                     
         BE    VKX                                                              
         CLI   ACTNUM,ACTREP                                                    
         BE    VKX                 THAT'S ALL                                   
*                                                                               
VK5      LA    R2,OFFLICDH         OFFICE LIST CODE                             
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         CLI   8(R2),C'$'          MUST START WITH $                            
         BNE   INVLFLD                                                          
         CLI   5(R2),2                                                          
         BE    VK6                                                              
         CLI   5(R2),3                                                          
         BE    VK7                                                              
         B     INVLFLD                                                          
VK6      MVI   CTUKPROG,0          $X TWO CHR OFFICE LIST CODE                  
         MVC   CTUKPROG+1(2),8(R2)                                              
         B     VKX                                                              
VK7      MVC   CTUKPROG(3),8(R2)   $XA THREE CHR OFFICE LIST CODE               
*                                                                               
VKX      MVC   SVKEY,KEY                                                        
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
**********************************************************************          
* VALIDATE RECORD ROUTINE                                            *          
**********************************************************************          
                                                                                
VR       LA    R6,ELEM             INITIALIZE PROFILE VALUE ELEMENT             
         USING CTPVD,R6                                                         
         MVI   BYTE,1              SET FIRST PASS (16 OFFICES)                  
         XC    ELEM,ELEM                                                        
         MVI   CTPVEL,CTPVELQ                                                   
         MVI   CTPVLEN,21                                                       
         CLI   ACTNUM,ACTADD                                                    
         BE    VR1                                                              
         MVI   ELCODE,CTPVELQ                                                   
         GOTO1 REMELEM                                                          
*                                                                               
VR1      BAS   RE,RO               READ OFFICE RECORDS                          
         LA    R2,OFFOC01H         POINT TO FIRST OFFICE CODE                   
         LA    R3,1                SET OFFICE RELATIVE NUMBER                   
*                                                                               
VR2      CLI   0(R2),8+L'OFFOC01   ENSURE ITS AN OFFICE CODE FIELD              
         BE    *+6                                                              
         DC    H'0'                                                             
         XC    WORK(24),WORK       INIT OFFICE ENTRY                            
         STC   R3,WORK                                                          
         CLI   5(R2),0             VALIDATE OFFICE CODE                         
         BE    VR10                                                             
         CLI   8(R2),C' '                                                       
         BNH   INVLFLD                                                          
         MVC   WORK+2(2),8(R2)                                                  
         CLI   WORK+3,0                                                         
         BNE   *+8                                                              
         MVI   WORK+3,C' '         WORK+2(2)=ALPHA OFFICE CODE                  
*                                                                               
VR4      LA    RF,OFFUSED+2        VALIDATE OFFICE CODE EXISTS                  
         LA    R1,1                                                             
VR5      CLC   WORK+2(2),0(RF)                                                  
         BE    VR6                                                              
         LA    R1,1(R1)            BUMP HEX CODE                                
         LA    RF,2(RF)                                                         
         CHI   R1,254                                                           
         BL    VR5                                                              
         CLI   WORK+3,C'?'         ONE CHR OFFICE WITHOUT OFFICE REC            
         BNE   INVLFLD                                                          
         CLI   WORK+2,C'A'         X? IS CONSIDERED OK                          
         BL    INVLFLD                                                          
         MVC   WORK+1(1),WORK+2    SET OFFICE CODE TO X                         
         MVC   WORK+4(20),=CL20'??????'                                         
         B     VR7                                                              
VR6      STC   R1,WORK+1           WORK+1(01)=OFFICE HEX CODE                   
         BAS   RE,GO               WORK+4(20)=OFFICE NAME                       
*                                                                               
VR7      LA    RE,OFFOC01H         CHECK FOR DUPLICATES                         
         CR    RE,R2                                                            
         BE    VR10                                                             
VR8      CLI   5(RE),0             BYPASS EMPTY FIELDS                          
         BE    VR9                                                              
         MVC   DUB(2),8(RE)                                                     
         CLI   DUB+1,0                                                          
         BNE   *+8                                                              
         MVI   DUB+1,C' '                                                       
         CLC   WORK+2(2),DUB                                                    
         BE    INVLFLD                                                          
VR9      LA    RE,OFFOC02-OFFOC01(RE)                                           
         CR    RE,R2                                                            
         BL    VR8                                                              
*                                                                               
VR10     SR    R0,R0               BUMP TO OFFICE NAME FIELD                    
         IC    R0,0(R2)                                                         
         LR    RE,R2                                                            
         AR    RE,R0                                                            
         CLI   0(RE),8+L'OFFON01   ENSURE ITS AN OFFICE NAME FIELD              
         BE    *+6                                                              
         DC    H'0'                                                             
         XC    8(L'OFFON01,RE),8(RE)                                            
         OI    6(RE),X'80'                                                      
         CLI   WORK+1,0            DO NOTHING IF NO OFFICE DEFINED              
         BE    VR12                                                             
         MVC   08(20,RE),WORK+4                                                 
*                                                                               
VR12     LA    RE,CTPVALUE-1(R3)   POINT TO SLOT IN ELEMENT                     
         MVC   0(1,RE),WORK+1      SET HEX OFFICE CODE                          
*                                                                               
VR14     LA    R3,1(R3)            BUMP TO NEXT OFFICE SLOT                     
         LA    R2,OFFOC02-OFFOC01(R2) BUMP TO NEXT SCREEN OFFICE FIELDS         
         CHI   R3,16                                                            
         BNH   VR2                                                              
         CLI   BYTE,1              TEST 1ST PASS                                
         BNE   VR24                                                             
*                                                                               
VR20     GOTO1 ADDELEM             ADD 1ST X'72' PROFILE VALUE ELEMENT          
         CLI   DMCB+12,0                                                        
         BE    VR22                                                             
         DC    H'0'                                                             
*                                                                               
VR22     XC    ELEM,ELEM           INITIALISE FOR SECOND ELEMENT                
         MVI   CTPVEL,CTPVELQ                                                   
         MVI   CTPVLEN,21                                                       
         MVI   CTPVPAGE,1          SET SECOND ELEMENT CODE                      
         MVI   BYTE,2              SET SECOND PASS (OFFICES 17-32)              
         LA    R3,1                                                             
         B     VR2                                                              
*                                                                               
VR24     OC    CTPVALUE,CTPVALUE   TEST IF SECOND SET OF OFFICE CODES           
         BZ    VRX                                                              
         GOTO1 ADDELEM             ADD 2ND X'72' PROFILE VALUE ELEMENT          
         CLI   DMCB+12,0                                                        
         BE    VRX                                                              
         DC    H'0'                                                             
*                                                                               
VRX      B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY RECORD                                                     *          
***********************************************************************         
                                                                                
DR       LA    R2,OFFOH01H         CLEAR SCREEN FIELDS                          
DR0      CLI   0(R2),0                                                          
         BE    DR1                                                              
         TM    1(R2),X'20'         CLEAR UNPROTECTED                            
         BZ    DR0A                                                             
         CLI   0(R2),8+L'OFFON01   CLEAR OFFICE NAME                            
         BNE   DR0B                                                             
         NOP   DR0A                                                             
DR0A     SR    R1,R1                                                            
         IC    R1,0(R2)                                                         
         AHI   R1,-9                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)                                                    
         OI    6(R2),X'80'                                                      
DR0B     SR    R1,R1                                                            
         IC    R1,0(R2)                                                         
         AR    R2,R1                                                            
         B     DR0                                                              
*                                                                               
DR1      L     R6,AIO                                                           
         USING CTUREC,R6                                                        
         MVI   ELCODE,CTPVELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CTPVD,R6                                                         
*                                                                               
         BAS   RE,RO               READ OFFICE RECORDS                          
         LA    R2,OFFOC01H         POINT TO FIRST OFFICE CODE                   
         LA    R3,1                SET OFFICE RELATIVE NUMBER                   
*                                                                               
DR2      CLI   0(R2),8+L'OFFOC01   ENSURE ITS AN OFFICE CODE FIELD              
         BE    *+6                                                              
         DC    H'0'                                                             
         XC    WORK(24),WORK       INIT OFFICE ENTRY                            
         STC   R3,WORK                                                          
         SR    R1,R1                                                            
         IC    R1,CTPVALUE-1(R3)                                                
         STC   R1,WORK+1           SET OFFICE HEX CODE                          
         LTR   R1,R1                                                            
         BZ    DR4                                                              
         SLL   R1,1                                                             
         LA    RE,OFFUSED(R1)                                                   
         MVC   WORK+2(2),0(RE)     SET OFFICE ALPHA CODE                        
         OC    WORK+2(2),WORK+2                                                 
         BNZ   DR3                                                              
         MVC   WORK+4(20),=CL20'??????'                                         
         MVC   WORK+2(2),=CL20'??'                                              
         CLI   WORK+1,C'A'         TEST OFFICE HEX CODE ALPHABETIC              
         BL    DR4                 NO                                           
         MVC   WORK+2(1),WORK+1    YES SHOW AS X?                               
         B     DR4                                                              
DR3      BAS   RE,GO               READ OFFICE RECORD TO GET NAME               
*                                                                               
DR4      MVC   8(2,R2),WORK+2      DISPLAY OFFICE ALPHA CODE                    
         OI    6(R2),X'80'                                                      
*                                                                               
DR6      SR    R0,R0               BUMP TO OFFICE NAME FIELD                    
         IC    R0,0(R2)                                                         
         LR    RE,R2                                                            
         AR    RE,R0                                                            
         CLI   0(RE),8+L'OFFON01   ENSURE ITS AN OFFICE NAME FIELD              
         BE    *+6                                                              
         DC    H'0'                                                             
         XC    8(L'OFFON01,RE),8(RE)                                            
         OI    6(RE),X'80'                                                      
         CLI   WORK+1,0            DO NOTHING IF NO OFFICE DEFINED              
         BE    DR8                                                              
         MVC   08(20,RE),WORK+4                                                 
*                                                                               
DR8      LA    R3,1(R3)            BUMP TO NEXT OFFICE SLOT                     
         LA    R2,OFFOC02-OFFOC01(R2) BUMP TO NEXT SCREEN LINE                  
         CHI   R3,16                                                            
         BNH   DR2                                                              
*                                                                               
DR10     SR    R0,R0               TEST IF SECOND X'72' ELEMENT                 
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    DRX                                                              
         CLI   CTPVEL,CTPVELQ                                                   
         BNE   DRX                                                              
         CLI   CTPVPAGE,1                                                       
         BNE   DRX                                                              
         LA    R3,1                SET OFFICE RELATIVE NUMBER                   
         B     DR2                                                              
*                                                                               
DRX      B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY KEY                                                         *         
***********************************************************************         
                                                                                
DK       L     R6,AIO              POINT TO RECORD                              
         USING CTUREC,R6                                                        
*                                                                               
DK1      LA    RE,SYSTAB           DISPLAY SYSTEM ID                            
         XC    OFFSYS,OFFSYS                                                    
         OI    OFFSYSH+6,X'80'                                                  
DK2      CLI   0(RE),C' '          SEARCH LIST OF VALID SYSTEMS                 
         BE    DK4                                                              
         CLC   7(1,RE),CTUKSYS                                                  
         BE    DK3                                                              
         LA    RE,L'SYSTAB(RE)                                                  
         B     DK2                                                              
DK3      MVC   OFFSYS(7),0(RE)                                                  
*                                                                               
DK4      MVC   OFFLICD(3),CTUKPROG DISPLAY OFFLIST ID                           
         CLI   CTUKPROG,0                                                       
         BNE   *+14                                                             
         MVC   OFFLICD(2),CTUKPROG+1                                            
         MVI   OFFLICD+2,0                                                      
         OI    OFFLICDH+6,X'80'                                                 
*                                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
**********************************************************************          
* RECORD JUST REPLACED OR ADDED                                      *          
**********************************************************************          
                                                                                
XRR      EQU   *                                                                
XRA      EQU   *                                                                
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DELETE/RESTORE A RECORD                                  *         
***********************************************************************         
                                                                                
DELREC   B     DR                  DISPLAY RECORD ON DELETE                     
*                                                                               
RESREC   L     R1,AIO              TEST RECORD IS DELETED                       
         TM    CTUSTAT-CTUREC(R1),X'80'                                         
         BO    DR                  DISPLAY RECORD ON RESTORE                    
         B     RECNOTD                                                          
         EJECT                                                                  
***********************************************************************         
* ONLINE LIST ROUTINE                                                           
***********************************************************************         
                                                                                
LR       LA    R5,LISTAR           R5=A(LIST SCREEN LINE)                       
         USING PLINED,R5                                                        
         BAS   RE,RO               READ OFFICE RECORDS                          
         CLI   MODE,PRINTREP                                                    
         BNE   LR02                                                             
         LA    R3,HEADING          SET UP REPORT HEADINGS                       
         ST    R3,SPECS                                                         
         LA    R3,HDRTN                                                         
         ST    R3,HEADHOOK                                                      
         LA    R5,P                R5=A(PRINT LINE)                             
*                                                                               
LR02     LA    R6,KEY                                                           
         USING CTUREC,R6                                                        
*                                                                               
LR03     OC    KEY(L'CTUKEY),KEY   FIRST TIME THROUGH                           
         BNZ   LR30                                                             
         MVI   CTUKTYP,CTUKTYPQ    C'U' REC TYPE                                
         MVI   CTUKPROG,X'00'                                                   
         MVI   CTUKPROG+1,C'$'     SET FOR FIRST OFFICE PROFILE                 
*                                                                               
LR04     LA    R2,LSTSYSH          SYSTEM                                       
         SR    R1,R1                                                            
         ICM   R1,1,5(R2)                                                       
         BZ    MISSFLD                                                          
         BCTR  R1,0                                                             
         LA    RE,SYSTAB                                                        
LR05     CLI   0(RE),C' '          SEARCH LIST OF VALID SYSTEMS                 
         BE    INVLFLD                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),8(R2)                                                    
         BE    LR06                                                             
         LA    RE,L'SYSTAB(RE)                                                  
         B     LR05                                                             
LR06     MVC   CTUKSYS,7(RE)                                                    
         MVC   INPSYS,7(RE)                                                     
*                                                                               
LR07     LA    R2,LSTOFLIH         OFFICE LIST CODE                             
         CLI   5(R2),0                                                          
         BE    LR10                                                             
         CLI   8(R2),C'$'          MUST START WITH $                            
         BNE   INVLFLD                                                          
         CLI   5(R2),2                                                          
         BE    LR08                                                             
         CLI   5(R2),3                                                          
         BE    LR09                                                             
         B     INVLFLD                                                          
LR08     MVI   CTUKPROG,0          $X TWO CHR OFFICE LIST CODE                  
         MVC   CTUKPROG+1(2),8(R2)                                              
         B     LR10                                                             
LR09     MVC   CTUKPROG(3),8(R2)   $XA THREE CHR OFFICE LIST CODE               
*                                                                               
LR10     LA    R6,KEY                                                           
         GOTO1 HIGH                FIRST RECORD                                 
         B     LR30                                                             
LR20     LA    R6,KEY                                                           
         GOTO1 SEQ                 NEXT RECORD                                  
*                                                                               
LR30     CLI   CTUKTYP,CTUKTYPQ    RECORD TYPE                                  
         BNE   LRX                                                              
         CLC   CTUKSYS,INPSYS      SYSTEM                                       
         BNE   LRX                                                              
         CLI   CTUKPROG,C'$'       THREE CHR OFFICE LIST $XA                    
         BH    LRX                                                              
         BE    LR35                                                             
         CLI   CTUKPROG,X'00'                                                   
         BNE   LR20                                                             
         CLI   CTUKPROG+1,C'$'     TWO CHR OFFICE LIST $X                       
         BE    LR35                                                             
         BL    LR20                                                             
         MVI   CTUKPROG,C'$'                                                    
         XC    CTUKPROG+1(2),CTUKPROG                                           
         B     LR10                                                             
                                                                                
LR35     CLC   CTUKAGY,AGENCY      AGENCY                                       
         BNE   LR20                                                             
*                                                                               
LR40     L     R6,AIO                                                           
         USING CTUREC,R6                                                        
         MVC   LISTAR,SPACES       CLEAR LIST LINE                              
         MVC   0(2,R5),CTUKPROG+1                                               
         CLI   CTUKPROG,C'$'                                                    
         BNE   *+10                                                             
         MVC   0(3,R5),CTUKPROG                                                 
         MVI   ELCODE,CTPVELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   LR20                BACK FOR NEXT IF NO ELEMENT                  
         USING CTPVD,R6                                                         
*                                                                               
LR50     LA    R3,CTPVALUE         POINT TO FIRST OFFICE CODE                   
         LA    R4,5(R5)                                                         
         ST    R4,DUB              START OF DISPLAY LINE                        
         LA    RE,L'LSTDAT1(R4)                                                 
         CLI   MODE,PRINTREP                                                    
         BNE   *+8                                                              
         LA    RE,L'P(R4)                                                       
         ST    RE,DUB+4            END OF DISPLAY LINE                          
         LA    R1,1                                                             
*                                                                               
LR60     MVC   0(2,R4),=C'  '      SET VALUE FOR OFFICE CODE N/D                
         CLI   0(R3),0             TEST ZERO HEX OFFICE CODE                    
         BE    LR62                                                             
         SR    RE,RE               INDEX INTO OFFICE CODE LIST                  
         IC    RE,0(R3)                                                         
         SLL   RE,1                                                             
         LA    RE,OFFUSED(RE)                                                   
         OC    0(2,RE),0(RE)                                                    
         BZ    LR61                                                             
         CLC   0(2,RE),SPACES                                                   
         BE    LR61                                                             
         MVC   0(2,R4),0(RE)       EXTRACT OFFICE CODE                          
         B     LR62                                                             
LR61     CLI   0(R3),C'A'          TEST IF VALID CHR CODE                       
         BL    LR62                                                             
         MVC   0(1,R4),0(R3)       X? MEANS NO OFFICE X RECORD                  
         MVI   1(R4),C'?'                                                       
*                                                                               
LR62     MVI   2(R4),C','                                                       
         LA    R3,1(R3)            BUMP TO NEXT OFFICE IN ELEMENT               
         LA    R1,1(R1)            BUMP NUM OF OFFICES                          
         LA    R4,3(R4)            BUMP TO NEXT DISPLAY LINE SLOT               
         C     R4,DUB+4                                                         
         BNL   LR64                END OF DISPLAY LINE                          
         CHI   R1,16                                                            
         BNH   LR60                                                             
*                                                                               
LR63     SR    R0,R0               TEST IF SECOND X'72' ELEMENT                 
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    LR64                                                             
         CLI   CTPVEL,CTPVELQ                                                   
         BNE   LR64                                                             
         CLI   CTPVPAGE,1                                                       
         BNE   LR64                                                             
         LA    R1,1                SET OFFICE RELATIVE NUMBER                   
         LA    R3,CTPVALUE         POINT TO FIRST OFFICE CODE                   
         B     LR60                                                             
*                                                                               
LR64     AHI   R4,-3               REMOVE TRAILING EMPTY ENTRIES                
         C     R4,DUB                                                           
         BL    LR65                                                             
         MVI   2(R4),0                                                          
         CLC   0(2,R4),=C'  '                                                   
         BNE   LR65                                                             
         XC    0(2,R4),0(R4)                                                    
         B     LR64                                                             
*                                                                               
         DROP  R6                                                               
*                                                                               
LR65     CLI   MODE,PRINTREP       TEST LIST/REPORT MODE                        
         BE    LR70                                                             
         GOTO1 LISTMON             SEND LINE TO SCREEN                          
         B     LR20                                                             
*                                                                               
LR70     EQU   *                   P HAS REPORT LINE FOR PRINTING               
*                                                                               
LR80     GOTO1 SPOOL,DMCB,(R8)                                                  
         B     LR20                                                             
*                                                                               
LRX      B     EXIT                                                             
         DROP  R5,R6                                                            
         EJECT                                                                  
***********************************************************************         
* HEADER ROUTINE                                                      *         
***********************************************************************         
                                                                                
HEADING  DS    0H                                                               
         SSPEC H1,1,REQUESTOR                                                   
         SSPEC H1,25,C'OFFICE LIST RECORDS'                                     
         SSPEC H2,25,C'-------------------'                                     
         SSPEC H1,55,AGYNAME                                                    
         SSPEC H2,55,AGYADD                                                     
         SSPEC H3,55,REPORT                                                     
         SSPEC H4,55,RUN                                                        
         SSPEC H5,55,PAGE                                                       
         DC    X'0'                                                             
*                                                                               
HDRTN    NTR1                                                                   
         LA    R4,H8                                                            
         USING PLINED,R4                                                        
         MVC   PLHEX(34),=C'CODE OFFICES                      '                 
         LA    R4,H9                                                            
         MVC   PLHEX(34),=C'---- -----------------------------'                 
         B     EXIT                                                             
         DROP  R4                                                               
*                                                                               
RELO     DS    A                   RELOCATION FACTOR                            
*                                                                               
INVLFLD  MVC   GERROR,=AL2(INVALID)                                             
         B     VSFMERR                                                          
*                                                                               
MISSFLD  MVC   GERROR,=AL2(MISSING)                                             
         B     VSFMERR                                                          
*                                                                               
NOTNFLD  MVC   GERROR,=AL2(NOTNUM)                                              
         B     VSFMERR                                                          
*                                                                               
RECNOTD  MVC   GERROR,=AL2(RECNTDEL)                                            
         B     VSFMERR                                                          
*                                                                               
VSFMERR  GOTO1 SFMERR                                                           
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO READ OFFICE RECORDS TO FIND AVAILABLE HEX CODES          *         
***********************************************************************         
                                                                                
RO       NTR1                                                                   
         MVI   OFFNEXT,0           SET NEXT AVAIL OFFICE CODE                   
         MVC   SVKEY,KEY                                                        
         MVC   SVAIO,AIO                                                        
         MVC   AIO,AIO2            SET I/O TO READ OFFICE RECORDS               
         LA    R6,KEY              SET KEY TO READ OFFICE RECORDS               
         USING COCREC,R6                                                        
         XC    COCKEY,COCKEY                                                    
         MVI   COCKTYP,COCKTYPQ    X'05' REC TYPE                               
         MVI   COCKSUB,COCKSUBQ    X'0B' REC SUB TYPE                           
         MVC   COCKAGY,AGENCY      AGENCY                                       
*                                                                               
         CLC   OFFUKEY,KEY         TEST IF ALREADY HAVE BUILT THIS LIST         
         BE    RO20                                                             
         MVC   OFFUKEY,KEY                                                      
         XC    OFFUSED(256),OFFUSED                                             
         XC    OFFUSED+256(256),OFFUSED+256                                     
         MVC   OFFUSED(2),=X'02FE'                                              
         MVC   OFFUSED+254(2),=X'03FE'                                          
*                                                                               
RO2      GOTO1 HIGH                FIRST RECORD                                 
         B     RO6                                                              
RO4      GOTO1 SEQ                 NEXT RECORD                                  
*                                                                               
RO6      L     R6,AIO                                                           
         USING COCREC,R6                                                        
         CLI   COCKTYP,COCKTYPQ    RECORD TYPE                                  
         BNE   RO20                                                             
         CLI   COCKSUB,COCKSUBQ    SUB TYPE                                     
         BNE   RO20                                                             
         CLC   COCKAGY,AGENCY      AGENCY                                       
         BNE   RO20                                                             
         CLI   COCKHEX,0           TEST IF ZERO HEX REC EXISTS                  
         BNE   RO8                                                              
         L     RE,AIO                                                           
         LA    RE,COCDATA-COCREC(RE)                                            
         MVC   OFFUSED(254),0(RE)                                               
         MVC   OFFUSED+254(254),254(RE)                                         
         B     RO20                                                             
*                                                                               
RO8      L     R6,AIO              POINT TO NEW RECORD                          
         USING COCREC,R6                                                        
         MVC   WORK+1(1),COCKHEX   SAVE HEX CODE                                
         MVC   WORK+2(2),=C'??'    INIT OFFICE CODE                             
         MVI   ELCODE,COCELCDQ                                                  
         BAS   RE,GETEL                                                         
         BNE   RO10                                                             
         USING COCELEM,R6                                                       
         MVC   WORK+2(2),COCCODE   SAVE OFFICE CODE                             
*                                                                               
RO10     SR    RF,RF               PUT OFFICE CODE IN LIST                      
         IC    RF,WORK+1                                                        
         SLL   RF,1                                                             
         LA    RF,OFFUSED(RF)                                                   
         MVC   0(2,RF),WORK+2                                                   
         B     RO4                 BACK FOR NEXT RECORD                         
*                                                                               
RO20     LA    RF,OFFUSED+2        GET NEXT AVAILABLE HEX CODE                  
         LA    R1,1                                                             
RO22     OC    0(2,RF),0(RF)                                                    
         BZ    RO24                                                             
         LA    R1,1(R1)                                                         
         LA    RF,2(RF)                                                         
         CHI   R1,254                                                           
         BL    RO22                                                             
         B     ROX                 EXIT WITH ZERO IF NO CODE AVAIL              
*                                                                               
RO24     STC   R1,OFFNEXT                                                       
*                                                                               
ROX      MVC   AIO,SVAIO                                                        
         MVC   KEY,SVKEY                                                        
         CLI   OFFNEXT,0           SET CC EQL IF NO OFFICE HEX FOUND            
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO READ OFFICE RECORD.                                      *         
* WORK+1(01)=OFFICE HEX CODE                                          *         
* WORK+2(02)=OFFICE ALPHA CODE                                        *         
* WORK+4(20)=OFFICE NAME INITIALISED TO ??????                        *         
***********************************************************************         
                                                                                
GO       NTR1                                                                   
         MVC   WORK+4(20),=CL20'??????'                                         
         MVC   SVKEY,KEY                                                        
         MVC   SVAIO,AIO                                                        
         MVC   AIO,AIO3            SET I/O TO READ OFFICE RECORD                
         LA    R6,KEY              SET KEY TO READ OFFICE RECORD                
         USING COCREC,R6                                                        
         XC    COCKEY,COCKEY                                                    
         MVI   COCKTYP,COCKTYPQ    X'05' REC TYPE                               
         MVI   COCKSUB,COCKSUBQ    X'0B' REC SUB TYPE                           
         MVC   COCKAGY,AGENCY      AGENCY                                       
         MVC   COCKHEX,WORK+1      HEX CODE                                     
*                                                                               
GO2      GOTO1 HIGH                READ OFFICE RECORD                           
         L     R6,AIO                                                           
         USING COCREC,R6                                                        
         CLC   COCKEY,KEY                                                       
         BNE   GOX                 EXIT IF OFFICE NOT FOUND                     
*                                                                               
GO4      MVI   ELCODE,COCELCDQ     GET OFFICE ELEMENT                           
         BAS   RE,GETEL                                                         
         BNE   GOX                                                              
         USING COCELEM,R6                                                       
         XC    WORK+4(20),WORK+4   OFFICE NAME                                  
         LA    R0,COCNAME-COCELEM                                               
         SR    R1,R1                                                            
         IC    R1,COCELLN                                                       
         SR    R1,R0                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK+4(0),COCNAME                                                
*                                                                               
GOX      MVC   AIO,SVAIO                                                        
         MVC   KEY,SVKEY                                                        
         CLC   WORK+4(20),=CL20'??????'                                         
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         SPACE 1                                                                
SYSTAB   DS    0CL8                                                             
         DC    C'ACCOUNT',C'A'                                                  
*&&UK*&& DC    C'MEDIA  ',C'M'                                                  
*&&US*&& DC    C'NETWORK',C'S'                                                  
*&&US*&& DC    C'REP    ',C'R'                                                  
*&&US*&& DC    C'SPOT   ',C'S'                                                  
SYSTABX  DC    C'       ',X'00'                                                 
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
*&&UK                                                                           
       ++INCLUDE CTGENOFC                                                       
*&&                                                                             
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE CTSFMFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFM9CD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFM9DD                                                       
NLSTLNQ  EQU   (LSTSELX-LSTSEL1)/(LSTSEL2-LSTSEL1)+1                            
         EJECT                                                                  
       ++INCLUDE CTSFMWORKD                                                     
*                                                                               
         ORG   SYSSPARE            TA0A17 STORAGE AREAS                         
SVAIO    DS    XL4                                                              
SVKEY    DS    XL25                                                             
OFFUKEY  DS    XL25                                                             
OFFUSED  DS    XL512                                                            
OFFNEXT  DS    X                                                                
INPSYS   DS    X                                                                
                                                                                
PLINED   DSECT                                                                  
PLHEX    DS    CL2                                                              
         DS    CL2                                                              
PLCODE   DS    CL2                                                              
         DS    CL3                                                              
PLNAME   DS    CL20                                                             
PEND     EQU   *                                                                
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005CTSFM17   05/01/02'                                      
         END                                                                    
