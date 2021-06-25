*          DATA SET RESFM06    AT LEVEL 069 AS OF 05/01/02                      
*PHASE T81806B,*                                                                
         TITLE 'T81806 - RESFM06 - ATHENA LIST/REPORT'                          
*                                                                               
*******************************************************************             
*                                                                 *             
*        RESFM06 (T81806) --- ATHENA ON-LINE LIST/REPORT          *             
*                                                                 *             
* --------------------------------------------------------------- *             
* UPDATE HISTORY:                                                 *             
*                                                                 *             
* JAN15/89 (MRR) --- MAKE RECORD I/O NON-LOCKING                  *             
*                                                                 *             
* JUN07/93 (BU ) --- UPGRADE TO DISPLAY AUR INFO BASED ON BIT 3   *             
*                    OF 'SFM PROFILE'                             *             
*                                                                 *             
* JUL14/93 (BU ) --- EXPAND AGENCY FILTER TO AGENCY/OFFICE        *             
*                                                                 *             
* JUL19/95 (SKU) --- FIX BIG FILE BUG                             *             
*                                                                 *             
* MAR20/97 (BU ) --- FIX NEGATIVE SPOT NUMBER PROBLEM             *             
*                                                                 *             
* JAN11/99 (BU ) --- FIX AGENCY OPTION ADDRESSABILITY             *             
*                                                                 *             
* JUN28/99 (AST) --- CHANGED SCREEN                               *             
*                                                                 *             
*                    **  END TOMBSTONE  **                        *             
*******************************************************************             
*                                                                               
T81806   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1806**,R7                                                    
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         XC    MYWORK(256),MYWORK                                               
         XC    MYWORK+256(256),MYWORK+256                                       
         SPACE                                                                  
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VKEY                                                             
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LIST                                                             
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DREC                                                             
         CLI   MODE,DISPKEY        GET DISPLAY LINES                            
         BE    DKEY                                                             
*                                                                               
         MVI   ERROR,INVACT                                                     
         B     TRAPERR                                                          
*                                                                               
         EJECT                                                                  
*              VALIDATE OPTION FIELDS                                           
         SPACE 3                                                                
VKEY     DS    0H                                                               
         MVI   ERROR,INVACT                                                     
         CLI   ACTNUM,ACTCHA                                                    
         BE    TRAPERR                                                          
         CLI   ACTNUM,ACTADD                                                    
         BE    TRAPERR                                                          
         CLI   ACTNUM,ACTDEL                                                    
         BE    TRAPERR                                                          
         CLI   ACTNUM,ACTREST                                                   
         BE    TRAPERR                                                          
         CLI   ACTNUM,ACTDIS                                                    
         BE    TRAPERR                                                          
*                                                                               
         MVI   SFMAUR,C'N'         SET 'NOT AUR USER'                           
         LR    R2,RA               USE R2 TO COVER THE ENTRY                    
         AH    R2,=AL2(SFMPROFS-CONHEADH)                                       
         USING SVDSECT,R2                                                       
         TM    SVPGPBIT,X'20'      3RD BIT (AUR USER) ON?                       
         BNO   VK050               NO                                           
         MVI   SFMAUR,C'Y'         YES - SET 'AUR USER'                         
*                                                                               
         DROP  R2                                                               
*                                                                               
VK050    EQU   *                                                                
         LA    RE,LSRSELH          ADDRESS DIRECTORY                            
         LA    R0,16                                                            
*                                                                               
VK100    CLI   8(RE),C' '          LOOK FOR FIRST/NEXT SELECTION                
         BH    XIT                                                              
         ZIC   RF,0(RE)                                                         
         AR    RE,RF                                                            
         IC    RF,0(RE)                                                         
         AR    RE,RF                                                            
         BCT   R0,VK100                                                         
         LA    R2,LSRSTAH                                                       
*                                                                               
         GOTO1 VALISTA                                                          
         CLC   =C'SRA',CONREC      SRA OR AUR?                                  
         BE    VK108               SRA                                          
         CLI   WORK+4,C'C'         COMBO STATION?                               
         BNE   VK108                                                            
         MVC   CONHEAD(36),=C'**  MUST REQUEST AURCOMBO REPORT  **'             
         LA    R2,LSRSTAH                                                       
         B     MYERR2                                                           
VK108    EQU   *                                                                
         MVC   OPTSTA,WORK                                                      
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC              READ STATION RECORD TO GET GROUP             
         L     R6,AIO                                                           
         MVC   OPTGRP,RSTAGRUP-RSTAREC(R6)                                      
*                                                                               
         MVI   ERROR,SECLOCK                                                    
         CLI   TWAOFFC,C'*'        DDS TERMINAL                                 
         BE    VK115                                                            
         CLI   TWAACCS,C'$'        TEST FOR STATION LIMITED ACCESS              
         BNE   VK115                                                            
         MVI   ELCODE,X'06'        GET ELEMENT FOR VALID SIGN-ON IDS            
         BAS   RE,GETEL                                                         
         B     *+8                                                              
VK110    BAS   RE,NEXTEL                                                        
         BNE   TRAPERR                                                          
         CLC   TWAORIG,10(R6)      SIGN-ON ID                                   
         BNE   VK110                                                            
*                                                                               
VK115    LA    R2,LSRPERH                                                       
         GOTO1 ANY                                                              
         MVI   ERROR,INVDATE                                                    
         GOTO1 SCANNER,DMCB,(R2),(1,WORK),C',=,-'                               
         CLI   4(R1),0                                                          
         BE    TRAPERR                                                          
         GOTO1 DATVAL,(R1),(2,WORK+12),DUB                                      
         OC    DMCB(4),DMCB                                                     
         BZ    TRAPERR                                                          
         GOTO1 DATCON,(R1),(0,DUB),(3,WORK+40)                                  
         MVC   OPTSTM,WORK+40                                                   
         MVC   OPTENM,WORK+40      IF NO END MONTH USE START                    
         CLI   WORK+1,0                                                         
         BE    VK120                                                            
*                                                                               
         GOTO1 DATVAL,(R1),(2,WORK+22),DUB                                      
         OC    DMCB(4),DMCB                                                     
         BZ    TRAPERR                                                          
         GOTO1 DATCON,(R1),(0,DUB),(3,WORK+50)                                  
         MVC   OPTENM,WORK+50                                                   
*                                                                               
         CLC   OPTSTM,OPTENM       START BEFORE END                             
         BH    TRAPERR                                                          
         ZIC   RE,WORK+40          MORE THAN 1 YEAR                             
         AH    RE,=H'1'                                                         
         STC   RE,WORK+40                                                       
         CLC   WORK+40(2),OPTENM                                                
         BNH   TRAPERR                                                          
VK120    MVC   OPTPSTM(4),OPTSTM   SET UP PRIOR YEAR                            
         ZIC   RE,OPTPSTM                                                       
         SH    RE,=H'1'                                                         
         STC   RE,OPTPSTM                                                       
         ZIC   RE,OPTPENM                                                       
         SH    RE,=H'1'                                                         
         STC   RE,OPTPENM                                                       
*                                                                               
VK200    MVC   CONHEAD(18),=C'** ERROR * INVALID'                               
         LA    R2,LSRDAYH          DAYPART                                      
         CLI   5(R2),0                                                          
         BE    VK400                                                            
         MVI   BYTE,C'Y'                                                        
         BAS   RE,SEPARATE         WORK SET WITH DPT - R5 # DPTS                
         STC   R5,OPTDPCNT         SAVE COUNT                                   
         LA    R3,WORK                                                          
VK220    ZIC   RE,ERRFLD                                                        
         LA    RE,1(RE)                                                         
         STC   RE,ERRFLD                                                        
*                                                                               
         CLI   0(R3),C'*'                                                       
         BNE   *+8                                                              
         MVI   0(R3),X'FE'         FOR SORT THE WAY ITS SAVED                   
         SR    R0,R0                                                            
         ICM   R0,1,SVDPTCNT                                                    
         BZ    VK250                                                            
         LA    RF,SVDPTFIL         IF DAYPARTS ALEADY VALIDATED SKIP            
VK240    CLC   0(2,R3),0(RF)       IF A MATCH NO NEED TO READ                   
         BE    VK320                                                            
         LA    RF,2(RF)                                                         
         BCT   R0,VK240                                                         
*                                                                               
VK250    CLI   0(R3),X'FE'         PROGRAM TYPE                                 
         BE    VK300                                                            
*                                                                               
         XC    KEY,KEY             DAYPART RECORD                               
         MVI   KEY,X'24'                                                        
         MVC   KEY+RDPTKREP-RDPTKEY(2),AGENCY                                   
         MVC   KEY+RDPTKDPT-RDPTKEY(1),0(R3)                                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   VKDPTER                                                          
         CLI   1(R3),0             IF PRIMARY DPT USE ALL 2NDARY DPTS           
         BE    VK320                                                            
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    VK280                                                            
VKDPTER  MVC   CONHEAD+19(7),=C'DAYPART'                                        
         MVC   CONHEAD+27(5),=C'FIELD'                                          
         OI    ERRFLD,X'F0'                                                     
         MVC   CONHEAD+34(1),ERRFLD                                             
         B     MYERR2                                                           
*                                                                               
VK260    BAS   RE,NEXTEL                                                        
         BNE   VKDPTER                                                          
VK280    CLC   1(1,R3),2(R6)                                                    
         BNE   VK260                                                            
         B     VK320                                                            
*                                                                               
VK300    XC    KEY,KEY                                                          
         MVI   KEY,X'25'                                                        
         MVC   KEY+RPGTKREP-RPGTKEY(2),AGENCY                                   
         MVC   KEY+RPGTKPGT-RPGTKEY(1),1(R3)                                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    VK320                                                            
         MVC   CONHEAD+19(12),=C'PROGRAM TYPE'                                  
         MVC   CONHEAD+32(5),=C'FIELD'                                          
         OI    ERRFLD,X'F0'                                                     
         MVC   CONHEAD+38(1),ERRFLD                                             
         B     MYERR2                                                           
*                                                                               
VK320    LA    R3,2(R3)                                                         
         BCT   R5,VK220                                                         
*                                                                               
         MVC   OPTDPT,WORK                                                      
         SPACE 2                                                                
* SORT ON DAYPART CODES                                                         
         ZIC   R5,OPTDPCNT                                                      
         CH    R5,=H'1'                                                         
         BNH   VK400                                                            
         GOTO1 XSORT,DMCB,OPTDPT,(R5),2,2,0                                     
*                                                                               
VK400    LA    R2,LSRLENH                                                       
         CLI   5(R2),0                                                          
         BE    VK500                                                            
         MVI   BYTE,C'N'                                                        
         BAS   RE,SEPARATE         WORK SET WITH DPT - R5 # DPTS                
         STC   R5,OPTLNCNT         SAVE COUNT                                   
         MVI   ERRFLD,0                                                         
         LA    R3,WORK                                                          
         LA    R4,OPTLEN                                                        
VK420    LA    R6,4                MAX LENGTH                                   
         LR    RE,R3               CHECK FOR M,1-9                              
         ZIC   RF,ERRFLD                                                        
         LA    RF,1(RF)                                                         
         STC   RF,ERRFLD                                                        
         B     *+12                                                             
*                                                                               
VK440    CLI   0(RE),0                                                          
         BE    VK460                                                            
         CLI   0(RE),C'M'          MINUTES?                                     
         BE    VK460                                                            
         CLI   0(RE),X'F0'                                                      
         BL    VKLENER                                                          
         CLI   0(RE),X'F9'                                                      
         BH    VKLENER                                                          
         LA    RE,1(RE)                                                         
         BCT   R6,VK440                                                         
         B     VK460                                                            
VKLENER  MVC   CONHEAD+19(6),=C'LENGTH'                                         
         MVC   CONHEAD+27(5),=C'FIELD'                                          
         OI    ERRFLD,X'F0'                                                     
         MVC   CONHEAD+33(1),ERRFLD                                             
         B     MYERR2                                                           
* PACK MINUTES                                                                  
VK460    LA    R1,4                                                             
         SR    R1,R6                                                            
         BNP   VKLENER                                                          
         BCTR  R1,0                                                             
         EX    R1,PACKLEN                                                       
         CVB   R0,DUB                                                           
         STH   R0,0(R4)                                                         
         CLI   0(RE),C'M'          MINUTES?                                     
         BNE   *+8                                                              
         OI    0(R4),X'80'         TURN ON MINUTES INDICATOR                    
         LA    R3,4(R3)                                                         
         LA    R4,2(R4)                                                         
         BCT   R5,VK420                                                         
         SPACE                                                                  
* SORT ON LENGTH CODES                                                          
         ZIC   R5,OPTLNCNT                                                      
         CH    R5,=H'1'                                                         
         BNH   VK500                                                            
         GOTO1 XSORT,DMCB,OPTLEN,(R5),2,2,0                                     
*                                                                               
VK500    LA    R2,LSROPTH                                                       
         MVI   OPTFMAT,C'L'        LENGTH DEFAULT                               
         CLI   5(R2),0             ANY DATA IN OPTION FIELD?                    
         BE    VK600               NO                                           
         BAS   RE,OPTVAL           YES - VALIDATE IT                            
*                                                                               
VK600    CLI   OPTTYP,0                                                         
         BNE   *+8                                                              
         MVI   OPTTYP,1            TYPE ALL IF ZERO                             
         CLC   OPTCDE(OPTLENE),SVOPTCDE                                         
         BE    VK620                                                            
         XC    SVBUFF(256),SVBUFF  CLEAR SAVED AREA'S & START OVER              
         XC    SVBUFF+256(256),SVBUFF+256                                       
         MVC   SVOPTCDE,OPTCDE                                                  
         MVC   SVOPTLT,LSROPT                                                   
         MVC   AGFLTFLG,AGOFFFLG                                                
VK620    MVC   SVFMAT,OPTFMAT                                                   
*                                                                               
VKEXT    DS    0H                                                               
XIT      XIT1                                                                   
PACKLEN  PACK  DUB,0(0,R3)                                                      
         EJECT                                                                  
SEPARATE ST    RE,FULL                                                          
         MVI   ERROR,INVALID                                                    
         XC    WORK,WORK                                                        
         XC    ELEMENT,ELEMENT                                                  
         LA    R3,WORK                                                          
         LA    R4,ELEMENT          INPUT FROM SCREEN LINE                       
         SPACE                                                                  
* SEPARATE KEY LETTER & DATA (IF ANY) MAX 4 ENTRIES                             
SR100    GOTO1 SCANNER,DMCB,(R2),(4,(R4)),0                                     
         CLI   4(R1),0                                                          
         BE    TRAPERR                                                          
         LA    R6,4                2 BLOCKS 4 ENTRIES                           
         SR    R5,R5               COUNTER                                      
         LA    R0,2                R0 WILL = 2 OR 4                             
         CLI   BYTE,C'Y'           DAYPART = Y                                  
         BE    *+8                                                              
         SLL   R0,1                R0 = LENGTH TO INCREMENET LINE               
SR200    SR    RE,RE                                                            
         ICM   RE,1,0(R4)                                                       
         BZ    SR300                                                            
         CR    RE,R0                                                            
         BH    TRAPERR                                                          
         BCTR  RE,0                                                             
         LA    RF,12(R4)                                                        
         EX    RE,MVCSTDL                                                       
         BCTR  R5,0                COUNT IT                                     
         AR    R3,R0               INCREMENET OUTPUT TABLE                      
*                                                                               
         LA    R4,32(R4)                                                        
         BCT   R6,SR200                                                         
SR300    LPR   R5,R5                                                            
SREXT    L     RE,FULL                                                          
         BR    RE                                                               
MVCSTDL  MVC   0(0,R3),0(RF)                                                    
         EJECT                                                                  
* ON ENTRY - R2 = A(FIELD HEADER)                                               
* ON XIT   - ALL OPTION FIELDS SET UP                                           
OPTVAL   NTR1                                                                   
         XC    AGOFFFLG,AGOFFFLG   CLEAR AGENCY FILTER FLAG                     
         MVI   ERRFLD,1                                                         
         L     R4,AIO2                                                          
         LA    R6,12               LOOP CONTROL - MAX ENTRIES                   
         SPACE                                                                  
* SEPARATE KEY LETTER & DATA (IF ANY)                                           
         GOTO1 SCANNER,DMCB,(R2),(12,(R4)),0                                    
         CLI   4(R1),0             ANY DATA?                                    
         BE    OVERR               NO  - SHOULD NOT HAVE HAPPENED               
*                                                                               
OV100    CLI   0(R4),0             TOP OF LOOP - ANY DATA?                      
         BZ    OVEXT               NO  - FINISHED                               
         CLI   0(R4),1             YES - ANYTHING IN FIRST HALF?                
         BNE   OVERR               NO  - ERROR                                  
         CLI   1(R4),0             ANYTHING IN SECOND HALF?                     
         BNE   OV200               YES                                          
*                                                                               
*                                  SUPPRESS SECONDARY DAYPARTS                  
         CLI   12(R4),C'X'         NO  - CHECK VALUE OF 1ST HALF                
         BNE   *+12                                                             
         MVI   OPNOSSD,C'Y'        SUPPRESS SECONDARY DAYPARTS                  
         B     OV1000                                                           
*                                                                               
*                                  STATION TOTALS ONLY                          
         CLI   12(R4),C'W'         STATION TOTALS ONLY?                         
         BNE   OVERR               NO  - ERROR                                  
         MVI   OPSTOT,C'Y'                                                      
         B     OV1000                                                           
*                                                                               
OV200    EQU   *                                                                
         CLI   SFMAUR,C'Y'         AUR USER?                                    
         BE    OX220               YES - FORCE AUR                              
         CLC   =C'SRA',CONREC      NO  - SRA?                                   
         BE    OV210               YES - PROCESS SRA                            
OX220    EQU   *                                                                
         CLI   1(R4),0                                                          
         BE    OVERR                                                            
         CLI   12(R4),C'$'         DOLLAR PROCESSOR INDICATOR?                  
         BNE   OV210               NO  -                                        
         CLI   OPT$$$,0            ONLY 1 TYPE ALLOWED                          
         BNE   OV$$$ER             ALREADY SELECTED - ERROR                     
*                                                                               
         CLI   22(R4),C'C'         COMBO $$$ REQUESTED?                         
         BE    OX240               YES                                          
         CLI   22(R4),C'B'         BOTH COMBO AND REQULAR?                      
         BNE   OV$$$ER             NO  - UNRECOGNIZED                           
OX240    EQU   *                                                                
         MVC   OPT$$$,22(R4)       SAVE TYPE ENTERED                            
         B     OV1000                                                           
*                                  RATING SERVICE: SRA RECORDS                  
OV210    EQU   *                                                                
         CLI   SFMAUR,C'Y'         AUR USER?                                    
         BE    OV250               YES - FORCE AUR                              
         CLC   =C'SRA',CONREC      SRA?                                         
         BNE   OV250               NO  - MUST BE AUR                            
*                                  RATING SERVICE: SRA RECORDS                  
         CLI   1(R4),0                                                          
         BE    OVERR                                                            
         CLI   12(R4),C'S'                                                      
         BNE   OV300                                                            
         CLI   OPTTYP,0            ONLY 1 TYPE ALLOWED                          
         BNE   OVTYPER                                                          
         ZIC   RE,1(R4)                                                         
         BCTR  RE,0                                                             
         LA    RF,SRVTBL                                                        
         LA    R0,7                                                             
OV220    EX    RE,CLCSRV                                                        
         BE    OV240                                                            
         LA    RF,3(RF)                                                         
         BCT   R0,OV220                                                         
         B     OVERR                                                            
*                                                                               
OV240    MVC   OPTCD(1),22(R4)                                                  
         MVI   OPTTYP,2                                                         
         B     OV1000                                                           
*                                                                               
OV250    EQU   *                   CONTRACT TYPE: AUR RECORDS                   
         CLI   1(R4),0                                                          
         BE    OVERR                                                            
         CLI   12(R4),C'T'                                                      
         BNE   OV300                                                            
         CLI   OPTTYP,0            ONLY 1 TYPE ALLOWED                          
         BNE   OVTYPER             ALREADY SELECTED - ERROR                     
*                                                                               
         MVC   OPTCD(1),22(R4)     SAVE TYPE ENTERED                            
         MVI   OPTTYP,2            SET RECORD TYPE FOR CONTRACT TYPE            
         B     OV1000                                                           
*                                  AS AT DATE                                   
OV300    CLI   12(R4),C'D'                                                      
         BNE   OV400                                                            
         GOTO1 DATVAL,DMCB,(0,22(R4)),WORK                                      
         OC    DMCB(4),DMCB                                                     
         BZ    OVERR                                                            
         GOTO1 DATCON,(R1),(0,WORK),(2,OPASDTE)                                 
         B     OV1000                                                           
*                                                                               
*                                  FORMAT (LENGTH/MONTHLY/QUARTERLY)            
OV400    CLI   12(R4),C'F'                                                      
         BNE   OV500                                                            
         CLI   1(R4),1                                                          
         BNE   OVERR                                                            
         CLI   22(R4),C'L'                                                      
         BE    OV420                                                            
         CLI   22(R4),C'M'                                                      
         BE    OV420                                                            
         CLI   22(R4),C'Q'                                                      
         BNE   OVERR                                                            
OV420    MVC   OPTFMAT,22(R4)                                                   
         B     OV1000                                                           
*                                                                               
OV500    EQU   *                                                                
         CLI   SFMAUR,C'Y'         AUR USER?                                    
         BE    OV550               YES - FORCE AUR                              
         CLC   =C'SRA',CONREC      SRA?                                         
         BNE   OV550               NO  - MUST BE AUR                            
*                                  CATEGORY:       SRA RECORDS                  
         CLI   12(R4),C'C'                                                      
         BNE   OV600                                                            
         CLI   OPTTYP,0            ONLY 1 TYPE ALLOWED                          
         BNE   OVTYPER                                                          
         XC    KEY,KEY                                                          
         MVI   KEY,X'0F'                                                        
         MVC   KEY+23(2),AGENCY                                                 
         MVC   KEY+25(2),22(R4)                                                 
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   OVERR                                                            
         MVI   OPTTYP,3                                                         
         MVC   OPTCD(2),22(R4)                                                  
         B     OV1000                                                           
*                                                                               
OV550    EQU   *                                                                
*                                  OFFICE:         AUR RECORDS                  
         CLI   12(R4),C'O'                                                      
         BNE   OV600                                                            
         CLI   OPTTYP,0            ONLY 1 TYPE ALLOWED                          
         BNE   OVTYPER             ALREADY ENTERED - ERROR                      
         XC    KEY,KEY                                                          
         MVI   KEY,X'04'           CHECK FOR OFFICE                             
         MVC   KEY+23(2),AGENCY    INSERT REP CODE                              
         MVC   KEY+25(2),22(R4)    INSERT OFFICE CODE                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BNE   OVERR               NO  - ERROR                                  
         MVI   OPTTYP,4            YES - SET TO OFFICE                          
         MVC   OPTCD(2),22(R4)     SAVE CODE                                    
         B     OV1000                                                           
*                                                                               
OV600    EQU   *                                                                
         CLI   SFMAUR,C'Y'         AUR USER?                                    
         BE    OV650               YES - FORCE AUR                              
         CLC   =C'SRA',CONREC      SRA?                                         
         BNE   OV650               NO  - MUST BE AUR                            
*                                  ADVERTISER:     SRA RECORDS                  
         DS    0H'0'                                                            
         CLI   12(R4),C'A'                                                      
         BNE   OVERR                                                            
         CLI   OPTTYP,0            ONLY 1 TYPE ALLOWED                          
         BNE   OVTYPER                                                          
         CLI   1(R4),4                                                          
         BH    OVERR                                                            
         MVC   OPTCD,SPACES        BLANK PRODUCT                                
         XC    KEY,KEY             MOVE IN ADV                                  
         MVI   KEY,X'08'                                                        
         MVC   KEY+21(4),SPACES                                                 
         ZIC   RE,1(R4)                                                         
         BCTR  RE,0                                                             
         EX    RE,MVCADV                                                        
         MVC   KEY+25(2),AGENCY                                                 
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   OVERR                                                            
         MVI   OPTTYP,4            SET SRA RECORD TYPE                          
         MVC   OPTCD(4),KEY+21     SAVE ADVERTISER CODE                         
         B     OV1000                                                           
MVCADV   MVC   KEY+21(0),22(R4)                                                 
*                                                                               
OV650    EQU   *                                                                
*                                  AGENCY:         AUR RECORDS                  
         DS    0H'0'                                                            
         CLI   12(R4),C'A'                                                      
         BNE   OVERR                                                            
         CLI   OPTTYP,0            ONLY 1 TYPE ALLOWED                          
         BNE   OVTYPER             ALREADY ENTERED - ERROR                      
         CLI   1(R4),7             MORE THAN 7 CHARACTERS ENTERED?              
         BH    OVERR               YES - ERROR                                  
         XC    KEY,KEY             CHECK AGENCY IN FILE                         
         MVI   KEY,X'0A'                                                        
         MVC   KEY+19(6),SPACES    SPACE-FILL AGENCY/AGYOFF                     
         GOTO1 =A(SETAGY),DMCB,(RC),(R4),RR=YES                                 
*                                  DEVELOP AGENCY/AGENCY OFFICE                 
         BNZ   OVERR               ERROR IN AGENCY CODE SETUP                   
         MVC   KEY+25(2),AGENCY                                                 
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   OVERR                                                            
         MVI   OPTTYP,3            SET AUR RECORD TYPE                          
         MVC   OPTCD,SPACES        SPACE-FILL FOR AGENCY OFFICE                 
         MVC   OPTCD(6),KEY+19     SAVE AGENCY CODE                             
         CLC   KEY+23(2),SPACES    CORPORATE AGENCY REQUESTED?                  
         BNE   OV1000              NO  - SPECIFIC OFFICE                        
OV660    EQU   *                                                                
         GOTO1 SEQ                 READ NEXT RECORD                             
         CLC   KEY(23),KEYSAVE     SAME THROUGH AGENCY?                         
         BNE   OV1000              NO  - ACCEPT INPUT                           
         CLC   KEY+25(2),KEYSAVE+25                                             
*                                  SAME REP?                                    
         BNE   OV660               NO  - GO BACK FOR NEXT RECORD                
         MVC   CONHEAD+27(31),=C'AGENCY OFFICE CODE REQUIRED    '               
         B     OVERR                                                            
*                                                                               
*                                  UP ERROR FIELD & NEXT BLOCK                  
OV1000   ZIC   RE,ERRFLD                                                        
         LA    RE,1(RE)                                                         
         STC   RE,ERRFLD                                                        
         LA    R4,32(R4)                                                        
         BCT   R6,OV100                                                         
*                                                                               
*                                  FILTER CHANGES MAY REQUIRE RE-READ           
OVEXT    B     XIT                                                              
*                                                                               
CLCSRV   CLC   0(0,RF),22(R4)                                                   
MVCOPLN  MVC   ELEMENT(0),8(R2)                                                 
SRVTBL   DC    C'ARB',C'NSI',C'SRC',C'BIR',C'TRC',C'MTD',C'RAM'                 
*                                                                               
OVERR    OI    ERRFLD,X'F0'                                                     
         MVC   CONHEAD+19(5),=C'FIELD'                                          
         MVC   CONHEAD+25(1),ERRFLD                                             
MYERR    LA    R2,LSROPTH                                                       
MYERR2   MVI   GENSTAT2,USMYOK                                                  
         GOTO1 ERREX2                                                           
*                                                                               
OVTYPER  MVC   CONHEAD+27(31),=C'TYPE,AGY,OFF - ONLY ONE ALLOWED'               
         B     OVERR                                                            
*                                                                               
OV$$$ER  MVC   CONHEAD+27(31),=C'$$$ TYPE: WRONG OR TWO ENTRIES '               
         B     OVERR                                                            
         EJECT                                                                  
*              DISPLAY KEY                                                      
         SPACE 3                                                                
DKEY     EQU   *                                                                
         MVI   KEY,0               END IF NOTHING TO DISPLAY                    
         LA    RE,LISTDIR          ADDRESS DIRECTORY                            
         LA    RF,LISTKEYS                                                      
         LH    R6,LKEY                                                          
         SR    R0,R0                                                            
         ICM   R0,1,LISTNUM                                                     
         BZ    XIT                                                              
*                                                                               
DK100    CLI   0(RE),C' '          LOOK FOR FIRST/NEXT SELECTION                
         BH    DK200                                                            
         LA    RE,6(RE)                                                         
         AR    RF,R6                                                            
         BCT   R0,DK100                                                         
         B     XIT                                                              
DK200    BCTR  R6,0                                                             
         EX    R6,MVCLKEY                                                       
         CLI   KEY,0                                                            
         BE    XIT                 BLANK SCREEN LINE                            
         BAS   RE,RDTWA                                                         
         B     XIT                                                              
MVCLKEY  MVC   KEY(0),0(RF)                                                     
         EJECT                                                                  
*              DISPLAY RECORD                                                   
         SPACE 3                                                                
DREC     EQU   *                                                                
         CLI   KEY,0                                                            
         BE    XIT                 BLANK SCREEN LINE                            
         MVC   DSROPT,SVOPTLT                                                   
         OI    DSROPTH+6,X'80'                                                  
         LA    R2,DSRLN1H                                                       
         BAS   RE,CLRSCRN                                                       
         USING DISPD,R2                                                         
*                                                                               
         CLI   KEY,X'FC'           STATION TOTAL DISPLAY                        
         BNE   DR100                                                            
         MVC   DDPTNAM,=C'STATION TOT'                                          
         BAS   RE,GETADISP         GET ADDR OF ACCUMS TO DISPLAY                
         B     DR200                                                            
*                                                                               
DR100    CLI   KEY,X'FE'           PROGRAM TYPE DISPLAY                         
         BNE   DR120                                                            
         MVC   DDPT(1),KEY+1                                                    
         MVC   DDPTNAM,KEY+3                                                    
         BAS   RE,GETADISP         GET ADDR OF ACCUMS TO DISPLAY                
         B     DR200                                                            
*                                                                               
DR120    CLI   KEY+1,X'FF'         DAYPART TOTAL DISPLAY                        
         BNE   DR140                                                            
         MVC   DDPT(1),KEY                                                      
         MVC   DDPTNAM(5),KEY+3                                                 
         BAS   RE,GETADISP         GET ADDR OF ACCUMS TO DISPLAY                
         B     DR200                                                            
*                                                                               
DR140    MVC   DDPT,KEY            DAYPART DISPLAY                              
         MVC   DDPTNAM,KEY+3                                                    
         BAS   RE,GETADISP         GET ADDR OF ACCUMS TO DISPLAY                
         SPACE                                                                  
* LENGTH DISPLAY                                                                
DR200    LA    R5,12                                                            
DR220    OC    0(2,R3),0(R3)       END OF LENGTHS                               
         BZ    DR500                                                            
         CLC   =X'FFFF',0(R3)      PLAN                                         
         BNE   *+14                                                             
         MVC   DLEN(3),=C'TOT'     MORE THAN 12 LENGTHS                         
         B     DR300                                                            
         CLI   SVFMAT,C'M'                                                      
         BE    DR260                                                            
         CLI   SVFMAT,C'Q'                                                      
         BE    DR280                                                            
         TM    0(R3),X'40'                                                      
         BZ    *+14                                                             
         MVC   DLEN(2),=C'XX'      MORE THAN 12 LENGTHS                         
         B     DR300                                                            
         TM    0(R3),X'80'                                                      
         BZ    DR240                                                            
         MVC   HALF,0(R3)                                                       
         NI    HALF,X'7F'                                                       
         EDIT  (2,HALF),(3,DLEN)                                                
         MVI   DLEN+3,C'M'                                                      
         B     DR300                                                            
*                                                                               
DR240    EDIT  (2,0(R3)),(3,DLEN)                                               
         B     DR300                                                            
*                                                                               
DR260    MVC   WORK(2),0(R3)                                                    
         MVI   WORK+2,0            DAY                                          
         GOTO1 DATCON,DMCB,(3,WORK),(6,DLEN)                                    
         MVC   DLEN+3(2),DLEN+4                                                 
         MVI   DLEN+5,C' '                                                      
         B     DR300                                                            
*                                                                               
DR280    MVC   DLEN(3),=C'QTR'                                                  
         MVC   DLEN+3(1),1(R3)                                                  
         OI    DLEN+3,X'F0'                                                     
*                                                                               
DR300    LA    R6,2(R3)                                                         
         LA    R4,DURB                                                          
         BAS   RE,PTRUNDL                                                       
         OI    6(R2),X'80'                                                      
         LA    R1,2(R3)                                                         
         LA    R6,DSTPUN                                                        
         BAS   RE,ADDUP            ADD TO DISPLAY TOTAL                         
         ZIC   RF,DLLMMCNT                                                      
         LA    RF,1(RF)                                                         
         STC   RF,DLLMMCNT                                                      
         ZIC   RE,0(R2)                                                         
         AR    R2,RE               NEXT SCREEN LINE                             
         LA    R3,14(R3)           NEXT LENGTH                                  
         BCT   R5,DR220                                                         
*                                                                               
DR500    CLI   DLLMMCNT,1                                                       
         BE    XIT                 ONLY ONE LEN - NO TOTAL                      
         MVC   DDPTNAM(5),=C'TOTAL'                                             
         LA    R6,DSTPUN                                                        
         LA    R4,DURB                                                          
         BAS   RE,PTRUNDL                                                       
         OI    6(R2),X'80'                                                      
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
* CALCULATE ADDR OF ACCUMS TO DISPLAY                                           
* INPUT  - KEY+2 = DAYPART NUMBER FOR MONTH DISPLAY                             
* INPUT  - KEY   = DAYPART/PROGRAM CODE FOR LENGTH DISPLAY                      
* OUTPUT - R3 SET TO ADDR OF ACCUM                                              
*          'CC' = LENGTH DISPLAY - 'CC' NE = MONTH DISPLAY                      
GETADISP ST    RE,FULL                                                          
         CLI   KEY,X'FC'           STATION TOTAL DISPLAY                        
         BNE   GP100                                                            
         LA    R3,SVSTALTL         ADDRESS OF 12 LENGTH BUCKETS                 
         CLI   SVFMAT,C'L'                                                      
         BE    GP120                                                            
         LA    R3,SVSTAMTL         ADDRESS OF 12 MONTH BUCKETS                  
         B     GP120                                                            
*                                                                               
GP100    L     R3,ATWASV1                                                       
         CLI   SVFMAT,C'L'                                                      
         BNE   *+8                                                              
         LA    R3,2688(R3)         POINT TO LENGTHS                             
         ZIC   R1,KEY+2                                                         
         MH    R1,=H'168'                                                       
         AR    R3,R1                                                            
*                                                                               
GP120    CLI   SVFMAT,C'Q'                                                      
         BNE   GPEXT                                                            
         SR    R4,R4                                                            
         LA    R5,12                                                            
         LA    R6,DQTRS                                                         
GP200    LA    RE,4                ADD UP MONTHS TO GET QUARTERS                
         CLI   1(R3),9             4TH QTR                                      
         BH    GP220                                                            
         BCTR  RE,0                                                             
         CLI   1(R3),6             3TH QTR                                      
         BH    GP220                                                            
         BCTR  RE,0                                                             
         CLI   1(R3),3             2TH QTR                                      
         BH    GP220                                                            
         BCTR  RE,0                1ST QTR                                      
GP220    LTR   R4,R4                                                            
         BZ    GP240                                                            
         CR    RE,R4                                                            
         BE    GP240                                                            
         LA    R6,14(R6)                                                        
GP240    LR    R4,RE                                                            
*                                                                               
         STCM  RE,3,0(R6)                                                       
         LA    R6,2(R6)                                                         
         LA    R1,2(R3)                                                         
         BAS   RE,ADDUP                                                         
         BCTR  R6,0                RESET R6                                     
         BCTR  R6,0                                                             
         LA    R3,14(R3)                                                        
         OC    0(2,R3),0(R3)                                                    
         BZ    *+8                                                              
         BCT   R5,GP200                                                         
         LA    R3,DQTRS                                                         
GPEXT    L     RE,FULL                                                          
         BR    RE                                                               
         EJECT                                                                  
*              LIST RECORDS                                                     
         SPACE 3                                                                
LIST     EQU   *                                                                
         CLI   SVSFMAUR,C'Y'       AUR USER?                                    
         BE    LIST2               YES - FORCE AUR                              
         CLC   =C'SRA',CONREC      SRA?                                         
         BNE   LIST2               NO  - MUST BE AUR                            
         LA    R2,LSRSELH                                                       
         BAS   RE,CLRSCRN                                                       
         MVI   USEIO,C'Y'                                                       
         LA    RE,BUFF             USE BUFF FOR 6144 TWA SAVE AREA              
         ST    RE,ATWASV1                                                       
         LA    RF,4000                                                          
         LA    RF,2144(RF)                                                      
         XCEF                                                                   
         MVI   NUMLINES,16         NUMBER OF SCREEN LINES                       
         MVI   NLISTS,16           NUMBER OF SCREEN LINES                       
         MVI   FRST,C'Y'                                                        
         LA    R3,SVDPTFIL         RESTART DPT AND LENGTH FILTER                
         ST    R3,SVADPTF          POINTER ADDRESSES                            
         LA    R3,SVLENFIL                                                      
         ST    R3,SVALENF                                                       
         SPACE                                                                  
*                                                                               
* READ ATHENA RECORDS                                                           
*      EXTRACT UNITS/DOLLARS BY :                                               
*        1.  MONTH                                                              
*        2.  LENGTH                                                             
*        3.  REGULAR                                                            
*                                                                               
         CLI   SVENDSW,C'Y'                                                     
         BE    RX920               JUST DISPLAY STATION TOTAL                   
         MVC   KEY,SVALLKY                                                      
         CLI   SVALLKY,0                                                        
         BNE   RX180                                                            
*                                                                               
         XC    KEY,KEY             CLEAR KEY                                    
         MVI   KEY,X'27'                                                        
         MVC   KEY+AREPE(2),AGENCY REP                                          
         MVC   KEY+AGRPE(15),SVGRP                                              
*                                                                               
         CLI   SVDPTCNT,0          DAYPART FILTER                               
         BE    RX180                                                            
         LA    R3,SVDPTFIL                                                      
         ST    R3,SVADPTF                                                       
RX120    CLI   0(R3),X'FE'         PROGRAM TYPE?                                
         BE    *+14                                                             
         MVC   KEY+ADPTE(2),0(R3)  1ST DAYPART - R3 ALSO SET BELOW              
         B     RX140                                                            
*                                                                               
         MVC   KEY+ADPTE(2),=X'FEFE'                                            
         MVC   KEY+APRGE(1),1(R3)                                               
*                                                                               
RX140    CLI   SVLENCNT,0          LENGTH FILTER                                
         BE    RX180                                                            
         LA    R3,SVLENFIL                                                      
         ST    R3,SVALENF                                                       
RX160    MVC   KEY+ASLNE(2),0(R3)  1ST LENGTH                                   
         MVC   KEY+AYME(2),SVPSTM                                               
*                                                                               
RX180    EQU   *                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLI   SVSEQSW,C'Y'                                                     
         BE    RX200                                                            
         B     RX220                                                            
*                                                                               
RX200    EQU   *                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
RX220    CLC   KEY(18),KEYSAVE     ID/REP/GROUP/STATION/TYPE                    
         BNE   RX900                                                            
*                                                                               
* FILTER RECORDS                                                                
         MVI   SVSEQSW,C'N'                                                     
         CLI   SVDPTCNT,0          DAYPART FILTER                               
         BZ    RX300                                                            
         L     R3,SVADPTF                                                       
         CLI   0(R3),X'FE'         PROGRAM TYPE                                 
         BE    RX260                                                            
         CLC   KEY+ADPTE(2),0(R3)                                               
         BE    RX300                                                            
         CLI   1(R3),0             IF BLANK PASS ALL 2NDARY DPT                 
         BNE   *+14                                                             
         CLC   KEY+ADPTE(1),0(R3)                                               
         BE    RX300                                                            
RX240    LA    R3,2(R3)            NEXT ENTRY - CHECK IF IT = TO KEY            
         CLI   0(R3),0                                                          
         BE    RX900               FINISH DAYPART FILTERS                       
         CLI   0(R3),X'FE'         PROGRAM TYPE                                 
         BE    RX280                                                            
         ST    R3,SVADPTF                                                       
         CLI   1(R3),0             IF BLANK PASS ALL 2NDARY DPT                 
         BNE   RX242                                                            
         CLC   KEY+ADPTE(1),0(R3)                                               
         BH    RX240               KEY GT NEXT FILTER                           
         BE    RX300                                                            
RX242    CLC   KEY+ADPTE(2),0(R3)                                               
         BH    RX240               KEY GT NEXT FILTER                           
         BE    RX300                                                            
         XC    KEY+APRGE(5),KEY+APRGE CLEAR TO END IF KEY                       
         B     RX120                                                            
*                                                                               
RX260    CLC   KEY+APRGE(1),1(R3)  PROGRAM TYPE                                 
         BE    RX300                                                            
RX262    LA    R3,2(R3)                                                         
         CLI   0(R3),0                                                          
         BE    RX900               FINISH DAYPART FILTERS                       
RX280    ST    R3,SVADPTF                                                       
         CLC   KEY+APRGE(1),1(R3)                                               
         BH    RX262               KEY GT NEXT FILTER                           
         BE    RX300                                                            
         XC    KEY+ASLNE(4),KEY+ASLNE CLEAR TO END IF KEY                       
         B     RX120                                                            
*                                                                               
RX300    CLI   SVLENCNT,0          LENGTH FILTER                                
         BZ    RX400                                                            
         CLC   KEY+ADPTE(3),SVALLKY+ADPTE                                       
         BE    *+12                                                             
         LA    R3,SVLENFIL         NEW DYAPART RESTART LENGTH TABLE             
         ST    R3,SVALENF                                                       
         L     R3,SVALENF                                                       
         CLC   KEY+ASLNE(2),0(R3)                                               
         BE    RX400                                                            
         BL    RX160                                                            
RX320    LA    R3,2(R3)                                                         
         OC    0(2,R3),0(R3)                                                    
         BNZ   RX340               FINISH LENGTH FILTERS                        
         ZIC   RF,KEY+APRGE                                                     
         LA    RF,1(RF)                                                         
         STC   RF,KEY+APRGE        BUMP DAYPART                                 
         XC    KEY+ASLNE(4),KEY+ASLNE CLEAR TO END IF KEY                       
         B     RX180                                                            
*                                                                               
RX340    ST    R3,SVALENF                                                       
         CLC   KEY+ASLNE(2),0(R3)                                               
         BH    RX320               KEY GT FILTER NEXT FILTER                    
         BNE   RX160                                                            
*                                                                               
RX400    CLC   KEY+AYME(2),SVPSTM                                               
         BNL   *+14                                                             
         MVC   KEY+AYME(2),SVPSTM                                               
         B     RX180                                                            
*                                                                               
         MVI   PRIORSW,C'Y'                                                     
         CLC   KEY+AYME(2),SVPENM                                               
         BNH   RX500                                                            
*                                                                               
         MVI   PRIORSW,C'N'                                                     
         CLC   KEY+AYME(2),SVSTM                                                
         BNL   *+14                                                             
         MVC   KEY+AYME(2),SVSTM                                                
         B     RX180                                                            
*                                                                               
         CLC   KEY+AYME(2),SVENM                                                
         BNH   RX500                                                            
         MVI   KEY+AYME,X'FF'     (CAN'T INCREMENT LENGTH- IF PLAN              
         B     RX180               IT COULD ALREADY BE 'FF')                    
*                                                                               
* CHECK BREAKS - ADD TO ACCUM AREAS & OUTPUT LINES IF BREAK                     
RX500    CLI   FRST,C'Y'                                                        
         BE    RX540                                                            
         CLC   KEY+ADPTE(5),SVALLKY+ADPTE DPT/LEN BREAK                         
         BE    *+16                                                             
         CLI   ADDLENSW,C'Y'                                                    
         BNE   *+8                                                              
         BAS   RE,ADDLEN                                                        
*                                                                               
         CLC   KEY+ADPTE(3),SVALLKY+ADPTE                                       
         BE    RX600                                                            
         CLI   SVSTOT,C'Y'         STATION TOTAL ONLY                           
         BE    RX600                                                            
         CLI   SVNOSSD,C'Y'                                                     
         BNE   RX520                                                            
         CLI   SVALLKY+ADPTE,X'FE' PROGRAM TYPE OUTPUT                          
         BE    RX520                                                            
         CLI   SVALLKY+ADPTE,X'FF' PSEUDO OUTPUT                                
         BE    RX520                                                            
         CLC   KEY+ADPTE(1),SVALLKY+ADPTE                                       
         BE    RX600                                                            
RX520    BAS   RE,LISTDPT          OUTPUT 1 LINE TO SCREEN                      
RX540    BAS   RE,READDPT          READ & DETERMINE IF NEW DPT FITS             
*    *** READDPT MAY XIT IF DPTS DON'T FIT ON SCREEN ***                        
*                                                                               
RX600    MVC   SVALLKY,KEY                                                      
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         XC    UNDL,UNDL                                                        
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
RX620    BAS   RE,NEXTEL                                                        
         BNE   RX640                                                            
*                                                                               
         OC    SVASDTE,SVASDTE                                                  
         BZ    *+14                                                             
         CLC   SVASDTE,2(R6)                                                    
         BL    RX620                                                            
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,UNACCM                                                      
         AH    RE,4(R6)                                                         
         STCM  RE,3,UNACCM                                                      
         ICM   RE,15,DLACCM                                                     
         A     RE,6(R6)                                                         
         STCM  RE,15,DLACCM                                                     
         MVI   ADDLENSW,C'Y'                                                    
         MVI   STATOTSW,C'Y'                                                    
         MVI   FRST,C'N'                                                        
         B     RX620                                                            
*                                                                               
RX640    BAS   RE,ADDMTH                                                        
         B     RX200                                                            
*                                                                               
RX900    CLI   FRST,C'Y'                                                        
         BE    RX920                                                            
         MVI   SVENDSW,C'Y'                                                     
         CLI   ADDLENSW,C'Y'                                                    
         BNE   *+8                                                              
         BAS   RE,ADDLEN                                                        
*                                                                               
         CLI   SVSTOT,C'Y'         STATION TOTAL ONLY                           
         BE    RX920                                                            
         BAS   RE,LISTDPT          OUTPUT 1 LINE TO SCREEN                      
RX920    CLI   STATOTSW,C'Y'       A STATION TOTAL TO PRINT                     
         BNE   RX940                                                            
         BAS   RE,LISTSTA                                                       
         MVI   SVENDSW,C'N'                                                     
         CLI   SVSTOT,C'Y'                                                      
         BE    RX940                                                            
         CLI   FRST,C'Y'                                                        
         BE    RX940                                                            
         BAS   RE,WRTWA                                                         
RX940    MVI   USEIO,C'N'                                                       
RXEXT    B     XIT                                                              
         EJECT                                                                  
LIST2    EQU   *                                                                
         LA    R2,LSRSELH                                                       
         BAS   RE,CLRSCRN                                                       
         MVI   USEIO,C'Y'                                                       
         LA    RE,BUFF             USE BUFF FOR 6144 TWA SAVE AREA              
         ST    RE,ATWASV1                                                       
         LA    RF,4000                                                          
         LA    RF,2144(RF)                                                      
         XCEF                                                                   
         MVI   NUMLINES,16         NUMBER OF SCREEN LINES                       
         MVI   NLISTS,16           NUMBER OF SCREEN LINES                       
         MVI   FRST,C'Y'                                                        
         LA    R3,SVDPTFIL         RESTART DPT AND LENGTH FILTER                
         ST    R3,SVADPTF          POINTER ADDRESSES                            
         LA    R3,SVLENFIL                                                      
         ST    R3,SVALENF                                                       
         SPACE                                                                  
*                                                                               
* READ AUR RECORDS                                                              
*      EXTRACT UNITS/DOLLARS BY :                                               
*        1.  MONTH                                                              
*        2.  LENGTH                                                             
*        3.  REGULAR                                                            
*        4.  COMBO           (AUR ONLY)                                         
*        5.  REGULAR+COMBO   (AUR ONLY)                                         
*                                                                               
         CLI   SVENDSW,C'Y'                                                     
         BE    LISX0560            JUST DISPLAY STATION TOTAL                   
         MVC   KEY,SVALLKY                                                      
         CLI   SVALLKY,0                                                        
         BNE   LISX0080                                                         
*                                                                               
         XC    KEY,KEY             CLEAR KEY                                    
         MVI   KEY,X'2C'                                                        
         MVC   KEY+AUREPE(2),AGENCY                                             
*                                  INSERT REP CODE                              
         MVC   KEY+AUGRPE(14),SVGRP                                             
*                                  INSERT GRP/STA+MED/TYPE/TYPE VALUE           
*                                                                               
         CLI   SVDPTCNT,0          DAYPART FILTER                               
         BE    LISX0080                                                         
         LA    R3,SVDPTFIL                                                      
         ST    R3,SVADPTF                                                       
LISX0020 CLI   0(R3),X'FE'         PROGRAM TYPE?                                
         BE    *+14                                                             
         MVC   KEY+AUDPTE(2),0(R3)                                              
*                                  1ST DAYPART - R3 ALSO SET BELOW              
         B     LISX0040                                                         
*                                                                               
         MVC   KEY+AUDPTE(2),=X'FEFE'                                           
         MVC   KEY+AUPRGE(1),1(R3)                                              
*                                                                               
LISX0040 CLI   SVLENCNT,0          LENGTH FILTER                                
         BE    LISX0080                                                         
         LA    R3,SVLENFIL                                                      
         ST    R3,SVALENF                                                       
LISX0060 MVC   KEY+AUSLNE(2),0(R3)                                              
*                                  1ST LENGTH                                   
         MVC   KEY+AUYME(2),SVPSTM                                              
*                                                                               
LISX0080 EQU   *                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLI   SVSEQSW,C'Y'                                                     
         BE    LISX0100                                                         
         B     LISX0120                                                         
*                                                                               
LISX0100 EQU   *                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
LISX0120 EQU   *                                                                
         CLI   AGFLTFLG,X'0'       ANY AGENCY OFFICE FLAG SET?                  
         BE    LISX0122            NO  - TEST 20 CHARS....                      
         CLI   AGFLTFLG,C'O'       YES - ANY AGENCY OFFICE CODE?                
         BNE   LISX0125            NO  - TEST 18 CHARS....                      
LISX0122 EQU   *                                                                
         CLC   KEY(20),KEYSAVE     NO  - ID/REP/GROUP/STATION/TYPE              
         BNE   LISX0540                                                         
         B     LISX0135                                                         
*                                                                               
LISX0125 EQU   *                                                                
*                                                                               
*   WHERE NO AGENCY OFFICE CODE HAS BEEN ENTERED, ONLY COMPARE THROUGH          
*      AGENCY CODE.                                                             
*                                                                               
         CLC   KEY(18),KEYSAVE     NO  - ID/REP/GROUP/STATION/TYPE              
         BNE   LISX0540                                                         
* FILTER RECORDS                                                                
LISX0135 EQU   *                                                                
         MVI   SVSEQSW,C'N'                                                     
         CLI   SVDPTCNT,0          DAYPART FILTER                               
         BZ    LISX0240                                                         
         L     R3,SVADPTF                                                       
         CLI   0(R3),X'FE'         PROGRAM TYPE                                 
         BE    LISX0180                                                         
         CLC   KEY+AUDPTE(2),0(R3)                                              
         BE    LISX0240                                                         
         CLI   1(R3),0             IF BLANK PASS ALL 2NDARY DPT                 
         BNE   *+14                                                             
         CLC   KEY+AUDPTE(1),0(R3)                                              
         BE    LISX0240                                                         
LISX0140 LA    R3,2(R3)            NEXT ENTRY - CHECK IF IT = TO KEY            
         CLI   0(R3),0                                                          
         BE    LISX0540            FINISH DAYPART FILTERS                       
         CLI   0(R3),X'FE'         PROGRAM TYPE                                 
         BE    LISX0220                                                         
         ST    R3,SVADPTF                                                       
         CLI   1(R3),0             IF BLANK PASS ALL 2NDARY DPT                 
         BNE   LISX0160                                                         
         CLC   KEY+AUDPTE(1),0(R3)                                              
         BH    LISX0140            KEY GT NEXT FILTER                           
         BE    LISX0240                                                         
LISX0160 CLC   KEY+AUDPTE(2),0(R3)                                              
         BH    LISX0140            KEY GT NEXT FILTER                           
         BE    LISX0240                                                         
         XC    KEY+AUPRGE(5),KEY+AUPRGE                                         
*                                  CLEAR TO END OF KEY                          
         B     LISX0020                                                         
*                                                                               
LISX0180 CLC   KEY+AUPRGE(1),1(R3) PROGRAM TYPE                                 
         BE    LISX0240                                                         
LISX0200 LA    R3,2(R3)                                                         
         CLI   0(R3),0                                                          
         BE    LISX0540            FINISH DAYPART FILTERS                       
LISX0220 ST    R3,SVADPTF                                                       
         CLC   KEY+AUPRGE(1),1(R3)                                              
         BH    LISX0200            KEY GT NEXT FILTER                           
         BE    LISX0240                                                         
         XC    KEY+AUSLNE(4),KEY+AUSLNE                                         
*                                  CLEAR TO END OF KEY                          
         B     LISX0020                                                         
*                                                                               
LISX0240 CLI   SVLENCNT,0          LENGTH FILTER                                
         BZ    LISX0300                                                         
         CLC   KEY+AUDPTE(3),SVALLKY+AUDPTE                                     
         BE    *+12                                                             
         LA    R3,SVLENFIL         NEW DAYPART RESTART LENGTH TABLE             
         ST    R3,SVALENF                                                       
         L     R3,SVALENF                                                       
         CLC   KEY+AUSLNE(2),0(R3)                                              
         BE    LISX0300                                                         
         BL    LISX0060                                                         
LISX0260 LA    R3,2(R3)                                                         
         OC    0(2,R3),0(R3)                                                    
         BNZ   LISX0280            FINISH LENGTH FILTERS                        
         ZIC   RF,KEY+AUPRGE                                                    
         LA    RF,1(RF)                                                         
         STC   RF,KEY+AUPRGE        BUMP DAYPART                                
         XC    KEY+AUSLNE(4),KEY+AUSLNE                                         
*                                  CLEAR TO END OF KEY                          
         B     LISX0080                                                         
*                                                                               
LISX0280 ST    R3,SVALENF                                                       
         CLC   KEY+AUSLNE(2),0(R3)                                              
         BH    LISX0260            KEY GT FILTER NEXT FILTER                    
         BNE   LISX0060                                                         
*                                                                               
LISX0300 CLC   KEY+AUYME(2),SVPSTM                                              
*                                  DATA EARLIER THAN PRIOR START?               
         BNL   *+14                NO                                           
         MVC   KEY+AUYME(2),SVPSTM                                              
*                                  YES - INSERT PRIOR START                     
         B     LISX0080            GO BACK AND RESTART                          
*                                                                               
         MVI   PRIORSW,C'Y'        SET 'PRIOR DATA' ON                          
         CLC   KEY+AUYME(2),SVPENM                                              
*                                  DATA AFTER PRIOR END?                        
         BNH   LISX0320            NO  - USE PRIOR DATA                         
*                                                                               
         MVI   PRIORSW,C'N'        YES - SET 'PRIOR DATA' OFF                   
         CLC   KEY+AUYME(2),SVSTM  DATA BEFORE CURRENT START?                   
         BNL   *+14                NO                                           
         MVC   KEY+AUYME(2),SVSTM  YES - INSERT CURRENT START                   
         B     LISX0080            GO BACK AND RESTART                          
*                                                                               
         CLC   KEY+AUYME(2),SVENM  DATA AFTER CURRENT END?                      
         BNH   LISX0320            NO  - USE CURRENT DATA                       
         MVI   KEY+AUYME,X'FF'    (CAN'T INCREMENT LENGTH- IF PLAN,             
*                                     IT COULD ALREADY BE 'FF')                 
         B     LISX0080                                                         
*                                                                               
* CHECK BREAKS - ADD TO ACCUM AREAS & OUTPUT LINES IF BREAK                     
*                                                                               
LISX0320 CLI   FRST,C'Y'                                                        
         BE    LISX0360                                                         
         CLC   KEY+AUDPTE(5),SVALLKY+AUDPTE                                     
*                                  DAYPART/LENGTH BREAK                         
         BE    *+16                                                             
         CLI   ADDLENSW,C'Y'                                                    
         BNE   *+8                                                              
         BAS   RE,ADDLEN                                                        
*                                                                               
         CLC   KEY+AUDPTE(3),SVALLKY+AUDPTE                                     
         BE    LISX0380                                                         
         CLI   SVSTOT,C'Y'         STATION TOTAL ONLY                           
         BE    LISX0380                                                         
         CLI   SVNOSSD,C'Y'                                                     
         BNE   LISX0340                                                         
         CLI   SVALLKY+AUDPTE,X'FE'                                             
*                                  PROGRAM TYPE OUTPUT                          
         BE    LISX0340                                                         
         CLI   SVALLKY+AUDPTE,X'FF'                                             
*                                  PSEUDO OUTPUT                                
         BE    LISX0340                                                         
         CLC   KEY+AUDPTE(1),SVALLKY+AUDPTE                                     
         BE    LISX0380                                                         
LISX0340 BAS   RE,LISTDPT          OUTPUT 1 LINE TO SCREEN                      
LISX0360 BAS   RE,READDPT          READ & DETERMINE IF NEW DPT FITS             
*    *** READDPT MAY XIT IF DPTS DON'T FIT ON SCREEN ***                        
*                                                                               
LISX0380 MVC   SVALLKY,KEY                                                      
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         XC    UNDL,UNDL                                                        
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
LISX0400 BAS   RE,NEXTEL                                                        
         BNE   LISX0520                                                         
*                                                                               
         OC    SVASDTE,SVASDTE                                                  
         BZ    *+14                                                             
         CLC   SVASDTE,2(R6)                                                    
         BL    LISX0400                                                         
*                                                                               
         CLI   SVOPT$$$,0          REGULAR DOLLARS REQUESTED?                   
         BNE   LISX0420            NO  - REGULAR NOT NEEDED                     
         TM    4(R6),X'80'         REGULAR DOLLARS IN ELEMENT?                  
         BNO   LISX0400            NO  - GO BACK FOR NEXT                       
         SR    RE,RE               YES                                          
         ICM   RE,3,UNACCM         ADD TO UNIT ACCUMULATOR                      
         AH    RE,5(R6)            USE FIRST BUCKETS                            
         STCM  RE,3,UNACCM                                                      
         ICM   RE,15,DLACCM                                                     
         A     RE,7(R6)            USE FIRST BUCKETS                            
         STCM  RE,15,DLACCM                                                     
         B     LISX0500                                                         
LISX0420 EQU   *                                                                
         CLI   SVOPT$$$,C'B'       BOTH $$$ TYPES REQUESTED?                    
         BNE   LISX0460            NO                                           
         TM    4(R6),X'80'         REGULAR DOLLARS IN ELEMENT?                  
         BNO   LISX0440            NO  - CHECK COMBO $$$                        
         SR    RE,RE               YES                                          
         ICM   RE,3,UNACCM         ADD TO UNIT ACCUMULATOR                      
         AH    RE,5(R6)            USE FIRST BUCKETS                            
         STCM  RE,3,UNACCM                                                      
         ICM   RE,15,DLACCM                                                     
         A     RE,7(R6)            USE FIRST BUCKETS                            
         STCM  RE,15,DLACCM                                                     
         TM    4(R6),X'40'         COMBO   DOLLARS IN ELEMENT?                  
         BNO   LISX0500            NO  - GO BACK FOR NEXT                       
         SR    RE,RE               YES                                          
         ICM   RE,3,UNACCM         ADD TO UNIT ACCUMULATOR                      
         AH    RE,11(R6)           USE SECOND BUCKETS                           
         STCM  RE,3,UNACCM                                                      
         ICM   RE,15,DLACCM                                                     
         A     RE,13(R6)           USE SECOND BUCKETS                           
         STCM  RE,15,DLACCM                                                     
         B     LISX0500                                                         
LISX0440 EQU   *                                                                
         TM    4(R6),X'40'         COMBO   DOLLARS IN ELEMENT?                  
         BNO   LISX0400            NO  - GO BACK FOR NEXT                       
*                                                                               
*   ABOVE SHOULD NEVER HAPPEN.  THIS WOULD INDICATE THAT THERE WAS              
*     A DOLLAR BUCKET WITH NEITHER REGULAR OR COMBO FIELDS                      
*                                                                               
         SR    RE,RE               YES                                          
         ICM   RE,3,UNACCM         ADD TO UNIT ACCUMULATOR                      
         AH    RE,5(R6)            USE FIRST BUCKETS                            
         STCM  RE,3,UNACCM                                                      
         ICM   RE,15,DLACCM                                                     
         A     RE,7(R6)            USE FIRST BUCKETS                            
         STCM  RE,15,DLACCM                                                     
         B     LISX0500                                                         
LISX0460 EQU   *                                                                
         CLI   SVOPT$$$,C'C'       COMBO $$ REQUESTED?                          
         BE    *+6                 YES                                          
         DC    H'0'                NO TYPE??????                                
         TM    4(R6),X'40'         COMBO   DOLLARS IN ELEMENT?                  
         BNO   LISX0400            NO  - GO BACK FOR NEXT                       
         TM    4(R6),X'80'         REGULAR DOLLARS IN ELEMENT?                  
         BNO   LISX0480            NO  - USE FIRST SET OF BUCKETS               
         SR    RE,RE               YES - USE SECOND SET OF BUCKETS              
         ICM   RE,3,UNACCM                                                      
         AH    RE,11(R6)                                                        
         STCM  RE,3,UNACCM                                                      
         ICM   RE,15,DLACCM                                                     
         A     RE,13(R6)                                                        
         STCM  RE,15,DLACCM                                                     
         B     LISX0500                                                         
LISX0480 EQU   *                                                                
         SR    RE,RE               USE FIRST  SET OF BUCKETS                    
         ICM   RE,3,UNACCM                                                      
         AH    RE,5(R6)                                                         
         STCM  RE,3,UNACCM                                                      
         ICM   RE,15,DLACCM                                                     
         A     RE,7(R6)                                                         
         STCM  RE,15,DLACCM                                                     
LISX0500 EQU   *                                                                
         MVI   ADDLENSW,C'Y'                                                    
         MVI   STATOTSW,C'Y'                                                    
         MVI   FRST,C'N'                                                        
         B     LISX0400                                                         
*                                                                               
LISX0520 BAS   RE,ADDMTH                                                        
         B     LISX0100                                                         
*                                                                               
LISX0540 CLI   FRST,C'Y'                                                        
         BE    LISX0560                                                         
         MVI   SVENDSW,C'Y'                                                     
         CLI   ADDLENSW,C'Y'                                                    
         BNE   *+8                                                              
         BAS   RE,ADDLEN                                                        
*                                                                               
         CLI   SVSTOT,C'Y'         STATION TOTAL ONLY                           
         BE    LISX0560                                                         
         BAS   RE,LISTDPT          OUTPUT 1 LINE TO SCREEN                      
LISX0560 CLI   STATOTSW,C'Y'       A STATION TOTAL TO PRINT                     
         BNE   LISX0580                                                         
         BAS   RE,LISTSTA                                                       
         MVI   SVENDSW,C'N'                                                     
         CLI   SVSTOT,C'Y'                                                      
         BE    LISX0580                                                         
         CLI   FRST,C'Y'                                                        
         BE    LISX0580                                                         
         BAS   RE,WRTWA                                                         
LISX0580 MVI   USEIO,C'N'                                                       
LISX0600 B     XIT                                                              
         EJECT                                                                  
* PUT RECORD TO BINARY SEARCH TABLE                                             
ADDMTH   NTR1                                                                   
         OC    UNDL,UNDL                                                        
         BZ    AMXIT                                                            
         LA    R3,WORK             BUILD AMOUNT RECORD                          
         USING DISMDSCT,R3                                                      
         XC    DISMWRK,DISMWRK                                                  
         MVC   DISMMTH,SVALLKY+AYME                                             
         CLI   PRIORSW,C'Y'                                                     
         BNE   AM100                                                            
         ZIC   RE,DISMMTH          ADD TO CURRENT YEAR BUCKET                   
         AH    RE,=H'1'                                                         
         STC   RE,DISMMTH                                                       
         MVC   DISMPUN,UNACCM      ADD TO PRIOR                                 
         MVC   DISMPDL,DLACCM                                                   
         B     *+16                                                             
AM100    MVC   DISMUN,UNACCM       ADD TO CURRENT                               
         MVC   DISMDL,DLACCM                                                    
         SPACE                                                                  
* ADD ACCUMS TO LENGTH ACCUM                                                    
         LA    R6,LENPUN                                                        
         LA    R1,DISMPUN                                                       
         BAS   RE,ADDUP                                                         
         SPACE                                                                  
         CLI   SVSTOT,C'Y'         STATION TOTAL ONLY                           
         BE    AM200                                                            
         SPACE                                                                  
* ADD MONTH DISPLAY RECORD                                                      
         LA    R1,DISMMTH          LEAVE R1 SET                                 
         L     R6,ATWASV1                                                       
         ZIC   R5,DPTNUM                                                        
         BAS   RE,ADDTOTBL         ADD A MONTH TO DISPLAY TABLE                 
         SPACE                                                                  
* ADD PRIMARY DAYPART MONTH DISPLAY RECORD                                      
         LA    R6,DPTTOTMH                                                      
         SR    R5,R5                                                            
         BAS   RE,ADDTOTBL         ADD A MONTH TO DISPLAY TABLE                 
         SPACE                                                                  
* NOW ADD TO STATIONS TOTAL MONTHS DISPLAY                                      
AM200    SR    R5,R5               NO DISPLACEMENT INTO TABLE                   
         LA    R1,DISMMTH          LEAVE R1 SET                                 
         LA    R6,SVSTAMTL         TABLE                                        
         BAS   RE,ADDTOTBL                                                      
AMXIT    B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
* PUT RECORD TO BINARY SEARCH TABLE - LENGTH                                    
* ONLY COME HERE IF NEW LENGTH - PRIOR/CURRNET SAME                             
ADDLEN   NTR1                                                                   
         OC    LENPUN(12),LENPUN                                                
         BZ    ALXIT                                                            
         MVC   LENLEN,SVALLKY+ASLNE                                             
         CLI   SVSTOT,C'Y'         STATION TOTAL ONLY                           
         BE    AL200                                                            
*                                                                               
         L     R6,ATWASV1                                                       
         LA    R6,2688(R6)         POINT PASS MONTH TABLE                       
         ZIC   R5,DPTNUM                                                        
         LA    R1,LENLEN                                                        
         BAS   RE,ADDTOTBL         ADD A MONTH TO DISPLAY TABLE                 
         SPACE                                                                  
* NOW ADD TO DAYPART ACCUM                                                      
         LA    R6,MDPACCM                                                       
         LA    R1,LENPUN                                                        
         BAS   RE,ADDUP                                                         
         SPACE                                                                  
* ADD ACCUMS TO STATION TOTAL ACCUM                                             
AL200    LA    R6,STLACCM                                                       
         LA    R1,LENPUN                                                        
         BAS   RE,ADDUP                                                         
         SPACE                                                                  
* ADD PRIMARY DAYPART LENGTH ACCUM                                              
         LA    R6,DPTTOTLN                                                      
         SR    R5,R5                                                            
         LA    R1,LENLEN                                                        
         BAS   RE,ADDTOTBL         ADD A MONTH TO DISPLAY TABLE                 
         SPACE                                                                  
* ADD LENGTH STATION TOTALS                                                     
         SR    R5,R5               NO DISPLACEMENT INTO TABLE                   
         LA    R6,SVSTALTL         TABLE                                        
         LA    R1,LENLEN                                                        
         BAS   RE,ADDTOTBL                                                      
         XC    LENACCM,LENACCM                                                  
ALXIT    B     XIT                                                              
         EJECT                                                                  
* OUTPUT 1 OR 2 LINES TO SCREEN                                                 
*      EQUATES ARE SAME FOR 'SRA' AND 'AUR' PROCESSING, SO THIS                 
*        ROUTINE HAS NO REFLECTION OF AN ALTERNATE KEY STRUCTURE                
*        FOR THE 2C KEYS.                                                       
*                                                                               
LISTDPT  NTR1                                                                   
         MVC   STKEY(34),KEY       SAVE KEY                                     
         LA    R3,LISTAR           TEMPORARY MOVE -TESTING                      
         USING LISTD,R3            DAYPART TOTALS                               
         OC    MDPACCM,MDPACCM                                                  
         BZ    OD220                                                            
         MVC   LISTAR,SPACES                                                    
         CLI   SVALLKY+APRGE,X'FC' PLAN DAYPART                                 
         BNE   *+14                                                             
         MVC   LDPTNAM(4),=C'PLAN'                                              
         B     OD200                                                            
         CLI   SVALLKY+ADPTE,X'FF' PSEUDO DAYPART                               
         BNE   OD100                                                            
         MVC   LDPTNAM(6),=C'PSEUDO'                                            
         B     OD200                                                            
*                                                                               
OD100    CLI   SVALLKY+ADPTE,X'FE' PROGRAM TYPE                                 
         BNE   OD120                                                            
         MVC   LDPT(1),SVALLKY+APRGE                                            
         MVC   LDPTNAM,CURPRG                                                   
         B     OD200                                                            
*                                                                               
OD120    MVC   LDPT(2),SVALLKY+ADPTE                                            
         MVC   LDPTNAM(5),CURDPT                                                
         ZIC   RE,TWOSDPT                                                       
         LA    RE,1(RE)                                                         
         STC   RE,TWOSDPT                                                       
*                                  SEPARATE NAMES WITH A /                      
         CLI   SVNOSSD,C'Y'                                                     
         BNE   *+12                                                             
         MVI   LDPT+1,C' '         ONLY OUTPUT PRIMARY                          
         B     OD200                                                            
*                                                                               
         CLC   CURSDPT,=C'XXXXX'                                                
         BE    OD200               NO 2NDARY DAYPART                            
         LA    R0,5                                                             
         LA    RE,LDPTNAM+4                                                     
OD140    CLI   0(RE),X'40'                                                      
         BNE   *+10                                                             
         BCTR  RE,0                                                             
         BCT   R0,OD140                                                         
         MVI   1(RE),C'/'                                                       
         MVC   2(5,RE),CURSDPT                                                  
*                                  EDIT 1 PAYPART                               
OD200    BAS   RE,LENTOT           OUTPUT LENGTH OR TOT                         
         LA    R6,MDPACCM                                                       
         LA    R4,LURB                                                          
         BAS   RE,PTRUNDL          OUTPUT UNITS/RATE/BILLING                    
         LA    R6,MDTACCM          ADD TO DAYPART TOTAL                         
         LA    R1,MDPACCM                                                       
         BAS   RE,ADDUP                                                         
         MVC   KEY(2),LDPT         BUILD DISPLAY PARMS                          
         MVC   KEY+2(1),DPTNUM                                                  
         MVC   KEY+3(11),LDPTNAM                                                
*        MVC   DMDSKADD(4),=X'0000FFFF'                                         
         XC    DMDSKADD,DMDSKADD   BIG FILE FIX                                 
         BAS   RE,GLISTMON                                                      
         ZIC   RE,DPTNUM           COUNT A DAYPART                              
         LA    RE,1(RE)                                                         
         STC   RE,DPTNUM                                                        
         XC    MDPACCM,MDPACCM                                                  
*                                  ALSO OUTPUT A MAIN DPT TOTAL                 
OD220    CLC   KEY+ADPTE(1),SVALLKY+ADPTE                                       
         BE    OD320                                                            
         CLI   TWOSDPT,1                                                        
         BNH   OD300                                                            
         OC    MDTACCM,MDTACCM                                                  
         BZ    OD300                                                            
         MVC   LISTAR,SPACES                                                    
         MVC   LDPT(1),SVALLKY+ADPTE                                            
         MVC   LDPTNAM(5),CURDPT                                                
         BAS   RE,LENTOT           OUTPUT LENGTH OR TOT                         
         LA    R4,LURB                                                          
         LA    R6,MDTACCM                                                       
         BAS   RE,PTRUNDL                                                       
         XC    KEY,KEY                                                          
         MVC   KEY(1),LDPT         BUILD DISPLAY PARMS                          
         MVI   KEY+1,X'FF'                                                      
         MVC   KEY+2(1),DPTNUM                                                  
         MVC   KEY+3(5),LDPTNAM                                                 
         BAS   RE,GLISTMON                                                      
         DROP  R3                                                               
*                                                                               
* ADD TO DISPLAY TABLE (2 ENTRIES LENGTH/MONTH)                                 
         L     R3,ATWASV1                                                       
         ZIC   R1,DPTNUM                                                        
         LR    RE,R1                                                            
         LA    RE,1(RE)            NEXT DAYPAT #                                
         STC   RE,DPTNUM                                                        
         MH    R1,=H'168'                                                       
         AR    R3,R1                                                            
         MVC   0(168,R3),DPTTOTMH                                               
         LA    R3,2688(R3)         LENGTH                                       
         MVC   0(168,R3),DPTTOTLN                                               
OD300    MVI   TWOSDPT,0                                                        
         XC    DPTTOTMH,DPTTOTMH                                                
         XC    DPTTOTLN,DPTTOTLN                                                
         XC    MDTACCM,MDTACCM                                                  
OD320    MVC   KEY,STKEY           RESET KEY                                    
         B     XIT                                                              
         EJECT                                                                  
* OUTPUT STATION TOTAL                                                          
LISTSTA  NTR1                                                                   
         OC    STLACCM,STLACCM                                                  
         BZ    STXIT                                                            
         MVC   LISTAR,SPACES                                                    
         LA    R3,LISTAR                                                        
         USING LISTD,R3                                                         
         MVC   LDPTNAM(11),=C'STATION TOT'                                      
         BAS   RE,LENTOT           OUTPUT LENGTH OR TOT                         
         LA    R6,STLACCM                                                       
         LA    R4,LURB                                                          
         BAS   RE,PTRUNDL                                                       
         XC    KEY,KEY                                                          
         MVI   KEY,X'FC'                                                        
*        MVC   DMDSKADD(4),=X'0000FFFF'                                         
         XC    DMDSKADD,DMDSKADD   BIG FILE FIX                                 
         MVI   STATOTSW,C'N'                                                    
         MVI   SVGRP,C'Y'          RESTART READ OF RECORDS                      
         BAS   RE,GLISTMON                                                      
STXIT    B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
* OUTPUT 1 LINE TO A SCREEN LINE                                                
* PARMS R4 = OUTPUT ADDRESS        R6 = A(/UNITS/DOLLARS)                       
PTRUNDL  NTR1                                                                   
         USING LISTFD,R4                                                        
*                                                                               
UD100    SR    R5,R5                                                            
         ICM   R5,3,0(R6)          ** UNITS **                                  
         BZ    UD120                                                            
         SLL   R5,16               REPROPAGATE SIGN                             
         SRA   R5,16                                                            
         EDIT  (R5),(7,LUPRI),MINUS=YES                                         
*                                                                               
UD120    ICM   R5,3,2(R6)                                                       
         BZ    UD200                                                            
         SLL   R5,16               REPROPAGATE SIGN                             
         SRA   R5,16                                                            
         EDIT  (R5),(7,LUCUR),MINUS=YES                                         
*                                                                               
UD140    EQU   *                                                                
*D140    SR    R3,R3               GET INDEX %                                  
*        SR    RE,RE                                                            
*        ICM   RE,3,0(R6)                                                       
*        BZ    UD200                                                            
*        ICM   R3,3,2(R6)                                                       
*        M     R2,=F'100'                                                       
*        SR    R2,R2                                                            
*        XC    FULL,FULL                                                        
*        STH   RE,FULL+2                                                        
*        D     R2,FULL             PRIOR DIVIDED BY CURRENT                     
*        SLL   R2,1                ROUND                                        
*        C     R2,FULL                                                          
*        BL    *+8                                                              
*        LA    R3,1(R3)                                                         
*        LTR   R3,R3                                                            
*        BNZ   *+14                                                             
*        MVC   LUIND(3),=C'MIN'                                                 
*        B     UD200                                                            
*        CH    R3,=H'1000'                                                      
*        BL    *+14                                                             
*        MVC   LUIND(3),=C'MAX'                                                 
*        B     UD200                                                            
*        EDIT  (R3),(3,LUIND)                                                   
*                                                                               
UD200    SR    R2,R2               ** RATE **                                   
         SR    RE,RE                                                            
         SR    R5,R5                                                            
         ICM   RE,3,0(R6)          PRIOR UNITS                                  
         BZ    UD220                                                            
         SLL   RE,16               REPROPAGATE SIGN                             
         SRA   RE,16                                                            
         ICM   R3,15,4(R6)         PRIOR DOLLARS                                
         BZ    UD220                                                            
         BNM   *+6                                                              
         LCR   R3,R3                                                            
         XC    FULL,FULL                                                        
         STH   RE,FULL+2                                                        
         D     R2,FULL                                                          
         SLL   R2,1                                                             
         C     R2,FULL                                                          
         BL    *+8                                                              
         LA    R3,1(R3)                                                         
         LR    R5,R3               SAVE PRIOR RATE                              
         ICM   RF,15,4(R6)         PRIOR DOLLARS                                
         BNM   *+6                                                              
         LCR   R3,R3                                                            
         EDIT  (R3),(7,LRPRI),MINUS=YES                                         
*                                                                               
UD220    SR    R2,R2               CURRENT                                      
         SR    RE,RE                                                            
         ICM   RE,3,2(R6)                                                       
         BZ    UD300                                                            
         SLL   RE,16               REPROPAGATE SIGN                             
         SRA   RE,16                                                            
         ICM   R3,15,8(R6)                                                      
         BZ    UD300                                                            
         BNM   *+6                                                              
         LCR   R3,R3                                                            
         XC    FULL,FULL                                                        
         STH   RE,FULL+2                                                        
         D     R2,FULL                                                          
         SLL   R2,1                                                             
         C     R2,FULL                                                          
         BL    *+8                                                              
         LA    R3,1(R3)                                                         
         LR    R2,R3                                                            
         ICM   RF,15,8(R6)                                                      
         BNM   *+6                                                              
         LCR   R3,R3                                                            
         EDIT  (R3),(7,LRCUR),MINUS=YES                                         
         LR    R3,R2                                                            
*                                                                               
UD240    EQU   *                                                                
*D240    LTR   R3,R3                                                            
*        BZ    UD300                                                            
*        LTR   R5,R5                                                            
*        BZ    UD300                                                            
*        ST    R5,FULL                                                          
*        M     R2,=F'100'                                                       
*        SR    R2,R2                                                            
*        D     R2,FULL                                                          
*        SLL   R2,1                                                             
*        C     R2,FULL                                                          
*        BL    *+8                                                              
*        LA    R3,1(R3)                                                         
*        LTR   R3,R3                                                            
*        BNZ   *+14                                                             
*        MVC   LRIND(3),=C'MIN'                                                 
*        B     UD300                                                            
*        CH    R3,=H'1000'                                                      
*        BL    *+14                                                             
*        MVC   LRIND(3),=C'MAX'                                                 
*        B     UD300                                                            
*        EDIT  (R3),(3,LRIND)                                                   
*                                                                               
UD300    ICM   R3,15,4(R6)         ** BILLING **                                
         BZ    UD320                                                            
         EDIT  (R3),(10,LBPRI),MINUS=YES                                        
*                                                                               
UD320    ICM   R3,15,8(R6)                                                      
         BZ    XIT                                                              
         EDIT  (R3),(10,LBCUR),MINUS=YES                                        
*                                                                               
UD340    EQU   *                                                                
*D340    ICM   RE,15,4(R6)                                                      
*        BZ    XIT                                                              
*        ST    RE,FULL                                                          
*        M     R2,=F'100'                                                       
*        SR    R2,R2                                                            
*        D     R2,FULL                                                          
*        SLL   R2,1                                                             
*        C     R2,FULL                                                          
*        BL    *+8                                                              
*        LA    R3,1(R3)                                                         
*        LTR   R3,R3                                                            
*        BNZ   *+14                                                             
*        MVC   LBIND(3),=C'MIN'                                                 
*        B     UDEXT                                                            
*        CH    R3,=H'1000'                                                      
*        BL    *+14                                                             
*        MVC   LBIND(3),=C'MAX'                                                 
*        B     UDEXT                                                            
*        EDIT  (R3),(3,LBIND)                                                   
UDEXT    B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
* OUTPUT EITHER LENGTH OR TOTAL                                                 
* PARM  R3 ADDR OUTPUT LINE                                                     
         USING LISTD,R3                                                         
LENTOT   ST    RE,FULL                                                          
         CLI   SVLENCNT,1                                                       
         BNE   LT200                                                            
         TM    SVLENFIL,X'80'                                                   
         BZ    LT100                                                            
         MVC   HALF,SVLENFIL                                                    
         NI    HALF,X'7F'                                                       
         EDIT  (2,HALF),(3,LLEN)                                                
         MVI   LLEN+3,C'M'                                                      
         B     LTEXT                                                            
*                                                                               
LT100    EDIT  (2,SVLENFIL),(3,LLEN)                                            
         B     LTEXT                                                            
*                                                                               
LT200    MVC   LLEN(3),=C'TOT'                                                  
LTEXT    L     RE,FULL                                                          
         BR    RE                                                               
         DROP  R3                                                               
         EJECT                                                                  
* CHECK FOR LAST LINE OF SCREEN                                                 
GLISTMON ST    RE,FULL                                                          
         MVI   SVSEQSW,C'N'                                                     
         ZIC   RE,NUMLINES                                                      
         BCTR  RE,0                SUBTRACT 1 SCREEN LINES LEFT                 
         STC   RE,NUMLINES                                                      
         LTR   RE,RE                                                            
         BNZ   GM100                                                            
         MVI   SVSEQSW,C'Y'                                                     
         MVI   USEIO,C'N'                                                       
         BAS   RE,WRTWA                                                         
GM100    GOTO1 LISTMON                                                          
         L     RE,FULL                                                          
         BR    RE                                                               
         SPACE 2                                                                
*  THIS ROUTINE IS FOR THE FUDGE LISTMON TO GET TO END OF SCREEN                
*   IT DOESN'T SET SVSEQSW ON                                                   
GLISTMOF ST    RE,FULL                                                          
         MVI   SVSEQSW,C'N'                                                     
         ZIC   RE,NUMLINES                                                      
         BCTR  RE,0                SUBTRACT 1 SCREEN LINES LEFT                 
         STC   RE,NUMLINES                                                      
         LTR   RE,RE                                                            
         BNZ   GF100                                                            
         MVI   USEIO,C'N'                                                       
         BAS   RE,WRTWA                                                         
GF100    GOTO1 LISTMON                                                          
         L     RE,FULL                                                          
         BR    RE                                                               
         EJECT                                                                  
* READ & CHECK IF SCREEN POSTION HAS ENOUGH ROOM TO OUTPUT ALL 2NDARY           
* -- NOTE -- IT EXPECTS RECORD TO REMAIN IN AIO2                                
READDPT  NTR1                                                                   
         LR    R5,RE               RE NZ NOT 1ST TIME FOR DPT                   
         MVC   SVALLKY,KEY         SAVE ATHENA KEY                              
         CLC   KEY+ADPTE(2),=X'FFFF'                                            
         BE    DNEXT               PSUEDO                                       
         CLC   SVALLKY+ADPTE(2),=X'FEFE'                                        
         BE    DN600               PROGRAM TYPE                                 
         XC    CURSDPT,CURSDPT                                                  
         CLC   SVALLKY+ADPTE(1),CURDPTC                                         
         BE    DN100               GET NEXT 2NDARY DPT                          
         SR    R5,R5                                                            
         XC    KEY,KEY                                                          
         MVI   KEY,X'24'                                                        
         MVC   KEY+24(2),AGENCY     REP                                         
         MVC   KEY+26(1),SVALLKY+ADPTE DAYPART                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
DN100    L     R6,AIO2                                                          
         MVC   CURDPT,RDPTNAME-RDPTKEY(R6)                                      
         MVC   CURDPTC,RDPTKDPT-RDPTKEY(R6)                                     
         XC    CURSDPT,CURSDPT                                                  
         CLI   SVNOSSD,C'Y'                                                     
         BE    DN720                                                            
         MVI   ELCODE,X'02'                                                     
         LTR   R5,R5                                                            
         BNZ   DN500                                                            
*                                                                               
         LA    R4,1                COUNTER (START AT 1 FOR DPT TOT)             
         BAS   RE,GETEL                                                         
         B     *+8                                                              
DN200    BAS   RE,NEXTEL                                                        
         BNE   DN400                                                            
*                                                                               
         LA    R4,1(R4)                                                         
         CLC   2(1,R6),SVALLKY+ASDTE                                            
         BNE   DN200                                                            
         MVC   CURSDPT,3(R6)                                                    
         B     DN200                                                            
*                                                                               
DN400    ZIC   RE,NUMLINES                                                      
         CR    R4,RE               DOES IT FIT ON SCREEN                        
         BNH   DN700                                                            
         BAS   RE,WRTWA                                                         
DN420    MVC   LISTAR,SPACES                                                    
         XC    KEY,KEY             LOOP UNTIL LISTMON XITS                      
*        MVC   DMDSKADD(4),=X'0000FFFF'                                         
         XC    DMDSKADD,DMDSKADD   BIG FILE FIX                                 
         BAS   RE,GLISTMOF         PASS LISTMON BLANK LINES TO END              
         BCT   R4,DN420            BUT DON'T SET SVSEQSW ON                     
         DC    H'0'                                                             
*                                                                               
DN500    BAS   RE,GETEL                                                         
         B     *+8                                                              
DN520    BAS   RE,NEXTEL                                                        
         BNE   DN700                                                            
*                                                                               
         CLC   2(1,R6),SVALLKY+ASDTE                                            
         BNE   DN520                                                            
         MVC   CURSDPT,3(R6)                                                    
         B     DN700                                                            
*                                                                               
DN600    XC    KEY,KEY             READ PROGRAM RECORD                          
         MVI   KEY,X'25'                                                        
         MVC   KEY+24(2),AGENCY                                                 
         MVC   KEY+26(1),SVALLKY+APRGE                                          
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    DN620                                                            
         MVC   CURPRG,=CL11'UNKNOWN'                                            
         B     DN640                                                            
DN620    MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
         L     R6,AIO2                                                          
         MVC   CURPRG,RPGTNAME-RPGTKEY(R6)                                      
DN640    SR    R5,R5                                                            
*                                                                               
DN700    OC    CURSDPT,CURSDPT                                                  
         BNZ   *+10                                                             
         MVC   CURSDPT,=C'XXXXX'                                                
DN720    MVC   KEY(34),SVALLKY     REREAD ATHENA KEY                            
         LTR   R5,R5               DON'T RE-READ IF DPT REC NOT READ            
         BNZ   DNEXT                                                            
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    DNEXT                                                            
         DC    H'0'                                                             
DNEXT    B     XIT                                                              
         EJECT                                                                  
* ADD 4 ACCUMS(2 HALFWORD/2 FULLWORD) - ASSUME NO HALFWORD ADRESSS              
* INPUT R6 -ADD TO                                                              
*       R1 -ADD FROM                                                            
ADDUP    SR    RF,RF                                                            
         SR    R0,R0                                                            
         ICM   RF,3,0(R6)                                                       
         ICM   R0,3,0(R1)                                                       
         AR    RF,R0                                                            
         STCM  RF,3,0(R6)          - PRIOR UNITS                                
         ICM   RF,3,2(R6)                                                       
         ICM   R0,3,2(R1)                                                       
         AR    RF,R0                                                            
         STCM  RF,3,2(R6)          - CURRENT UNITS                              
         ICM   RF,15,4(R6)                                                      
         ICM   R0,15,4(R1)                                                      
         AR    RF,R0                                                            
         STCM  RF,15,4(R6)         - PRIOR DOLLARS                              
         ICM   RF,15,8(R6)                                                      
         ICM   R0,15,8(R1)                                                      
         AR    RF,R0                                                            
         STCM  RF,15,8(R6)          - CURRENT DOLLARS                           
         BR    RE                                                               
         EJECT                                                                  
* ADD TO DISPLAY TABLE - FIND MATCH ON LEN/MTH OR 0 AREA THAN ADD               
* TABLE WILL BE KEEP IN SORTED ORDER                                            
* PARMS R6 = START OF TABLE (AREA TO BE ADDED TO)                               
*       R5 = DISP OF DPT INTO TABLE (SOMETIMES 0 START AT 1ST POINT)            
*       R1 = INFO TO BE ADDED                                                   
ADDTOTBL NTR1                                                                   
         MH    R5,=H'168'          ALL TABLES HAVE 168 BYTE MENBERS             
         AR    R6,R5               ADD TO LOCATION                              
         LA    R5,154(R6)          R5 = LAST OUTPUT POSTION                     
         LA    R0,12                                                            
DB100    OC    0(2,R6),0(R6)       0 ADD HERE                                   
         BZ    DB160                                                            
         CLC   0(2,R1),0(R6)                                                    
         BE    DB160                                                            
         BL    DB200                                                            
         LA    R6,14(R6)           NEXT ENTRY                                   
         BCT   R0,DB100                                                         
         LR    R6,R5               LAST ENTRY                                   
DB120    MVC   0(2,R6),0(R1)       LENGTH HIGEST                                
DB140    OI    0(R6),X'40'         MULT LEN INDICATOR                           
*                                                                               
DB160    MVC   0(2,R6),0(R1)                                                    
         LA    R1,2(R1)                                                         
         LA    R6,2(R6)                                                         
         BAS   RE,ADDUP                                                         
         B     XIT                                                              
         SPACE                                                                  
* THERE CAN BE MORE THAN 12 LENGTHS (NOT MONTHS) SO PLACE IN NEW LENGTH         
*        1 - IF LAST ENTRY EMPTY MOVE GT LENGTHS TO END OF TABLE                
*          - MOVE IN NEW ENTRY (DB300)                                          
*        2 - IF LAST ENTRY EXIST & LEN ENDED ON LAST ENTRY CHECK LENGTH         
*          - LT MOVE IN NEW LENGTH & ADD GT/= JUST ADD TO LAST ENTRY            
*        3 - IF LAST ENTRY EXIST & LEN DIDN'T END ON LAST ENTRY                 
*          - ADD NEXT TO LAST ENTRY TO LAST ENTRY, MOVE GT ENTRY TO END         
*          - OF TABLE THAN MOVE IN NEW ENTRY (DB220/DB300)                      
DB200    OC    0(2,R5),0(R5)                                                    
         BZ    DB300                                                            
         CH    R0,=H'1'            ON LAST ENTRY                                
         BNE   DB220                                                            
         LR    R6,R5                                                            
         NI    0(R6),X'7F'         TURN OFF MULT IN TO COMPARE                  
         CLC   0(2,R1),0(R6)                                                    
         BL    DB120               MOVE IN NEW LENGTH & ADD                     
         B     DB140               JUST ADD                                     
*                                                                               
DB220    ST    R1,DUB              SAVE R1                                      
         ST    R6,DUB+4            SAVE R6                                      
         LR    R1,R5                                                            
         SH    R1,=H'14'           POINT TO 2ND TO LAST ENTRY                   
         MVC   0(2,R5),0(R1)       MOVE IN NEW LAST LENGTHH                     
         LA    R1,2(R1)            ADD FROM 2ND LAST ENTRY                      
         LA    R6,2(R5)            ADD TO LAST ENTRY                            
         BAS   RE,ADDUP                                                         
         OI    0(R5),X'40'         MULT IND                                     
         SH    R5,=H'14'           MOVE NEW ENTRIES INTO 2ND TO LAST            
         L     R1,DUB              RESET R1                                     
         L     R6,DUB+4            RESET R6                                     
*                                                                               
DB300    LR    R0,R5                                                            
         SR    R0,R6               R0 = # TO MOVE                               
         BZ    DB400                                                            
         BCTR  R5,R0               R5 = MOVE FROM                               
         LA    R4,14(R5)           R4 = MOVE TO                                 
DB320    MVC   0(1,R4),0(R5)                                                    
         BCTR  R4,0                                                             
         BCTR  R5,0                                                             
         BCT   R0,DB320                                                         
*                                                                               
DB400    MVC   0(14,R6),0(R1)      MOVE IN NEW ENTRY                            
         B     XIT                                                              
         EJECT                                                                  
* READ/WRITE 1 - 6144 BYTE SAVED TWA AREA'S                                     
WRTWA    NTR1                                                                   
         LA    RE,=C'DMWRT '                                                    
         B     TW100                                                            
*                                                                               
RDTWA    NTR1                                                                   
         LA    RE,=C'DMREAD'                                                    
TW100    ST    RE,DMCB                                                          
         LA    R2,BUFF             USE BUFF FOR 6144 TWA SAVE AREA              
         ST    R2,ATWASV1                                                       
TW200    L     RE,ATWA                                                          
         MVI   DMCB+8,2                                                         
         MVI   DMCB+9,0                                                         
         MVC   DMCB+10(2),2(RE)    TERMINAL NUMBER                              
         GOTO1 DATAMGR,DMCB,,=C'TEMPSTR',,(R2),0                                
         CLI   8(R1),0             BLOW ON ANY ERROR HERE                       
         BE    XIT                                                              
         DC    H'0'                                                             
         EJECT                                                                  
CLRSCRN  NTR1                                                                   
         L     R6,ASPOOLD                                                       
         USING SPOOLD,R6                                                        
*                                                                               
*        ROUTINE TO CLEAR THE SCREEN                                            
*        FROM FIELD AT R2                                                       
*                                                                               
         SR    RE,RE                                                            
*                                                                               
CS010    IC    RE,0(R2)                                                         
         SH    RE,=H'9'                                                         
         EX    RE,CSCLC                                                         
         BE    CS020                                                            
         EX    RE,CSOC                                                          
         BZ    CS020                                                            
         EX    RE,CSXC                                                          
         OI    6(R2),X'80'                                                      
*                                                                               
CS020    LA    R2,9(RE,R2)                                                      
         CLI   0(R2),9                                                          
         BH    CS010                                                            
         B     XIT                                                              
*                                                                               
CSCLC    CLC   8(0,R2),SPACES                                                   
CSOC     OC    8(0,R2),8(R2)                                                    
CSXC     XC    8(0,R2),8(R2)                                                    
         DROP  R6                                                               
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE                                                                  
TRAPERR  GOTO1 ERREX                                                            
         SPACE 3                                                                
RELO     DS    A                                                                
         LTORG                                                                  
         EJECT                                                                  
LISTD    DSECT                                                                  
LDPT     DS    CL2                                                              
         DS    CL1                                                              
LDPTNAM  DS    CL11                                                             
         DS    CL1                                                              
LLEN     DS    CL4                                                              
         DS    CL1                                                              
LURB     DS    0CL1                                                             
         SPACE 2                                                                
DISPD    DSECT                                                                  
         DS    CL8                                                              
DDPT     DS    CL2                                                              
         DS    CL1                                                              
DDPTNAM  DS    CL11                                                             
         DS    CL1                                                              
DLEN     DS    CL5                                                              
         DS    CL2                                                              
DURB     DS    0CL1                                                             
         SPACE 2                                                                
LISTFD   DSECT                                                                  
LUPRI    DS    CL7                                                              
         DS    CL1                                                              
LUCUR    DS    CL7                                                              
*        DS    CL1                                                              
*UIND    DS    CL3                                                              
         DS    CL1                                                              
LRPRI    DS    CL7                                                              
         DS    CL1                                                              
LRCUR    DS    CL7                                                              
*        DS    CL1                                                              
*RIND    DS    CL3                                                              
         DS    CL1                                                              
LBPRI    DS    CL10                                                             
         DS    CL1                                                              
LBCUR    DS    CL10                                                             
*        DS    CL1                                                              
*BIND    DS    CL3                                                              
         EJECT                                                                  
* RESFMWORKD                                                                    
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* RESFMFFD                                                                      
* RESFME6D                                                                      
* RESFMF6D                                                                      
* REGENSTA                                                                      
* REGENATNA                                                                     
* REGENSDD                                                                      
* REGENDPT                                                                      
* REGENPGT                                                                      
***      PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
***      PRINT ON                                                               
         EJECT                                                                  
****     PRINT OFF                                                              
       ++INCLUDE RESFMFFD                                                       
         ORG   CONHEADH+3508       USE LAST 12 BYTES OF SCREEN AREA             
SFMPROFS DS    CL12                                                             
         ORG                                                                    
       ++INCLUDE DDGENTWA                                                       
***      PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE RESFME6D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE RESFMF6D                                                       
         EJECT                                                                  
       ++INCLUDE REGENSTA                                                       
         EJECT                                                                  
       ++INCLUDE REGENATNA                                                      
         EJECT                                                                  
       ++INCLUDE REGENSDD                                                       
         EJECT                                                                  
       ++INCLUDE REGENDPT                                                       
         EJECT                                                                  
       ++INCLUDE REGENPGT                                                       
         EJECT                                                                  
****     PRINT OFF                                                              
       ++INCLUDE RESFMWORKD                                                     
****     PRINT ON                                                               
         EJECT                                                                  
* THE NEXT 250 BYTES ARE USED BY BOTH LIST & DISPLAY FOR TEMPORARY              
* AREAS THAT CAN NOT BE PASSED THRU GENCON ROUTINES                             
* IT IS CLEAR ON EVERY ENTRY INTO THE PROGRAM (250 BYTES)                       
         ORG   SYSSPARE                                                         
*                                                                               
*                  --- LIST WORK AREA ---                                       
         DS    0F                                                               
MYWORK   DS    0CL512                                                           
UNDL     DS    0CL6                                                             
DLACCM   DS    F                   DOLLAR ACCUM:  REGULAR DOLLARS               
UNACCM   DS    H                   UNIT ACCUM:    REGULAR UNITS                 
PRIORSW  DS    CL1                 Y = RECORD IS PRIOR MONTH                    
ADDLENSW DS    CL1                 Y = ADD A LENGTH DISPLAY REC                 
DPTNUM   DS    CL1                 DAYPARTS POITION ON SCREEN                   
FRST     DS    CL1                 1ST TIME FOR SCREEN                          
LENACCM  DS    0CL14               A LENGTH ACUMMULATOR                         
LENLEN   DS    CL2                 LENGTH                                       
LENPUN   DS    CL2                 PRIOR UNITS                                  
LENUN    DS    CL2                 UNITS                                        
LENPDL   DS    CL4                 PRIOR DOLLARS                                
LENDL    DS    CL4                 DOLLARS                                      
MDPACCM  DS    0CL12               A DAYPART ACUMMULATOR                        
MDPPUN   DS    CL2                 PRIOR UNITS                                  
MDPUN    DS    CL2                 UNITS                                        
MDPPDL   DS    CL4                 PRIOR DOLLARS                                
MDPDL    DS    CL4                 DOLLARS                                      
MDTACCM  DS    0CL12               A DAYPART TOTAL ACUMMULATOR                  
MDTPUN   DS    CL2                 PRIOR UNITS                                  
MDTUN    DS    CL2                 UNITS                                        
MDTPDL   DS    CL4                 PRIOR DOLLARS                                
MDTDL    DS    CL4                 DOLLARS                                      
OPTCDE   DS    0CL50                                                            
OPTGRP   DS    CL2                 GROUP                                        
OPTSTA   DS    CL5                 STATION                                      
OPTTYP   DS    CL1                 ATHENA REC TYPE(EX1=ALL,2=CONTYPE)           
OPTCD    DS    CL7                 CONTYPE/AGENCY/OFFICE   CODE                 
OPTPSTM  DS    CL2                 PRIOR START MONTH                            
OPTPENM  DS    CL2                 PRIOR END MONTH                              
OPTSTM   DS    CL2                 START MONTH                                  
OPTENM   DS    CL2                 END MONTH                                    
OPTDPT   DS    CL8                 DAYPARTS                                     
         DS    CL1                 ENDING 0                                     
OPTDPCNT DS    CL1                 DAYPART COUNTER                              
OPTLEN   DS    CL8                 LENGTHS                                      
         DS    CL2                 ENDING 0                                     
OPTLNCNT DS    CL1                 LENGTH COUNTER                               
OPNOSSD  DS    CL1                 SUPPRESS 2NDARY DAYPART                      
OPSTOT   DS    CL1                 STATION TOTAL ONLY                           
OPASDTE  DS    CL2                 COMPRESSED AS AT DATE                        
OPT$$$   DS    CL1                 REGULAR/COMBO/BOTH $$                        
SFMAUR   DS    CL1                 Y  =  THIS REP IS AUR USER                   
OPTLENE  EQU   *-OPTCDE                                                         
OPTFMAT  DS    CL1                 FORMAT (EX. M=MONTH)                         
CURPRG   DS    CL11                CURRENT PROGRAM NAME                         
CURDPTC  DS    CL1                 CURRENT DAYPART CODE                         
CURDPT   DS    CL5                 CURRENT DAYPART NAME                         
CURSDPT  DS    CL5                 CURRENT SECONDARY DAYPART NAME               
TWOSDPT  DS    CL1                 TWO SECONDARY DAYPARTS                       
NUMLINES DS    CL1                 NUMBER OF LINES LEFT ON SCREEN               
ERRFLD   DS    CL1                 CURRENT FIELDON MULTPLY LINE                 
AGOFFFLG DS    CL1                                                              
*                                  X'0'  -  NO AGENCY FILTER                    
*                                  C'C'  -  AGENCY CODE ONLY                    
*                                  C'O'  -  AGENCY CODE AND OFFICE              
         DS    0F                                                               
DPTTOTMH DS    CL168               12 14 BYTE PRIMARY DPT TOTS (MONTH)          
DPTTOTLN DS    CL168               12 14 BYTE PRIMARY DPT TOTS (LENGTH)         
STKEY    DS    CL34                SAVE KEY                                     
         SPACE 2                                                                
*                --- DISPLAY WORK AREA ---                                      
         ORG   MYWORK                                                           
DSTBUFF  DS    0CL12               DISPLAY TOTAL                                
DSTPUN   DS    CL2                 PRIOR UNITS                                  
DSTUN    DS    CL2                 UNITS                                        
DSTPDL   DS    CL4                 PRIOR DOLLARS                                
DSTDL    DS    CL4                 DOLLARS                                      
DQTRNUM  DS    CL1                 CURRENT QUARTER NUMBER (1-4)                 
DLLMMCNT DS    CL1                 COUNT OF LENGTHS/MONTHS TO DISPLAY           
DQTRS    DS    CL64                4 QTR1 TOTALS                                
         ORG   MYWORK+512                                                       
         EJECT                                                                  
* THIS AREA IS SAVED FOR PRECEEDING ENTERS (750 BYTES)                          
SVBUFF   DS    0CL512                                                           
SVALLKY  DS    CL34                                                             
*                                                                               
*    ATHENA EQUATES                                                             
*                                                                               
AREPE    EQU   RATNKREP-RATNKEY                                                 
AGRPE    EQU   RATNKGRP-RATNKEY                                                 
ASTAE    EQU   RATNKSTA-RATNKEY                                                 
ATPEE    EQU   RATNKTPE-RATNKEY                                                 
ATCPE    EQU   RATNKTCD-RATNKEY                                                 
ASERE    EQU   RATNKSER-RATNKEY                                                 
ACATE    EQU   RATNKCAT-RATNKEY                                                 
APRDE    EQU   RATNKPRD-RATNKEY                                                 
ADPTE    EQU   RATNKDPT-RATNKEY                                                 
ASDTE    EQU   RATNKSDT-RATNKEY                                                 
APRGE    EQU   RATNKPRG-RATNKEY                                                 
ASLNE    EQU   RATNKSLN-RATNKEY                                                 
ASLN2E   EQU   RATNKSLN-RATNKEY+1                                               
AYME     EQU   RATNKYM-RATNKEY                                                  
*                                                                               
*    A.U.R. EQUATES                                                             
*                                                                               
AUREPE   EQU   RAURKREP-RAURKEY    REP                                          
AUGRPE   EQU   RAURKGRP-RAURKEY    GROUP                                        
AUSTAE   EQU   RAURKSTA-RAURKEY    STATION                                      
AUTPEE   EQU   RAURKTPE-RAURKEY    RECORD TYPE                                  
AUTCPE   EQU   RAURKTCD-RAURKEY    KEY: TYPE=ALL                                
AUCTYPE  EQU   RAURKCTP-RAURKEY    KEY: TYPE=CONTRACT TYPE                      
AUCAGYE  EQU   RAURKAGY-RAURKEY    KEY: TYPE=AGENCY                             
AUOFFE   EQU   RAURKOFF-RAURKEY    KEY: TYPE=OFFICE                             
AUDPTE   EQU   RAURKDPT-RAURKEY    DAYPART                                      
AUSDTE   EQU   RAURKSDT-RAURKEY    SUBDAYPART                                   
AUPRGE   EQU   RAURKPRG-RAURKEY    PROGRAM TYPE                                 
AUSLNE   EQU   RAURKSLN-RAURKEY    SPOT LENGTH: BYTE 1                          
AUSLN2   EQU   RAURKSLN-RAURKEY+1  SPOT LENGTH: BYTE 2                          
AUYME    EQU   RAURKYM-RAURKEY     YEAR/MONTH                                   
*                                                                               
SVOPTCDE DS    0CL50                                                            
SVGRP    DS    CL2                 GROUP                                        
SVSTA    DS    CL5                 STATION                                      
SVTTYP   DS    CL1                 REC TYPE(EX1=ALL,2=CONTYPE)                  
SVTCD    DS    CL7                 PRODUCT/SERVICE/CATEGORY CODE                
SVPSTM   DS    CL2                 PRIOR START                                  
SVPENM   DS    CL2                 PRIOR END                                    
SVSTM    DS    CL2                 START                                        
SVENM    DS    CL2                 END                                          
SVDPTFIL DS    CL8                 DAYPART/PROGRAM TYPE FILTERS                 
         DS    CL1                 ENDING 0                                     
SVDPTCNT DS    CL1                 DAYPART/PROGRAM TYPE COUNT                   
SVLENFIL DS    CL8                 LENGTH FILTERS                               
         DS    CL2                 ENDING 0                                     
SVLENCNT DS    CL1                 LENGTH COUNT                                 
SVNOSSD  DS    CL1                 SUPPRESS 2NDARY DAYPART                      
SVSTOT   DS    CL1                 STATION TOTAL ONLY                           
SVASDTE  DS    CL2                 COMPRESSED AS AT DATE                        
SVOPT$$$ DS    CL1                 REGULAR/COMBO/BOTH $$                        
SVSFMAUR DS    CL1                 Y  =  THIS REP IS AUR USER                   
SVTLENE  EQU   *-SVOPTCDE                                                       
*                                                                               
SVFMAT   DS    CL1                 FORMAT (EX. M=MONTH)                         
STATOTSW DS    CL1                 Y = PRINT A STATION TOTAL                    
SVENDSW  DS    CL1                 Y = STILL MUST DISPLAY STA TOT               
SVSEQSW  DS    CL1                 Y = DO A SEQ READ ON NEXT SCREEN             
         DS    0F                                                               
SVSTAMTL DS    CL168               12 MONTH TOTALS STATION (LEN =12)            
SVSTALTL DS    CL168               12 LENGTH TOTALS STATION (LEN =14)           
SVADPTF  DS    A                   LAST DAYPART FILTER READ                     
SVALENF  DS    A                   LAST LENGTH FILTER READ                      
ATWASV1  DS    A                   6144 EXTRA TWA AREA                          
STLACCM  DS    0CL12               A STATION TOTAL ACUMMULATOR                  
STLPUN   DS    CL2                 PRIOR UNITS                                  
STLUN    DS    CL2                 UNITS                                        
STLPDL   DS    CL4                 PRIOR DOLLARS                                
STLDL    DS    CL4                 DOLLARS                                      
SVOPTLT  DS    CL29                                                             
AGFLTFLG DS    CL1                 AGENCY FILTER FLAG                           
*                                  X'0'  -  NO AGENCY FILTER                    
*                                  C'C'  -  AGENCY CODE ONLY                    
*                                  C'O'  -  AGENCY CODE AND OFFICE              
         EJECT                                                                  
* DISPLAY DSECTS                                                                
DISMDSCT DSECT                                                                  
DISMWRK  DS    0CL14               MONTH DISPALY REC                            
DISMMTH  DS    CL2                 MONTH                                        
DISMPUN  DS    CL2                 PRIOR UNITS                                  
DISMUN   DS    CL2                 UNITS                                        
DISMPDL  DS    CL4                 PRIOR DOLLARS                                
DISMDL   DS    CL4                 DOLLARS                                      
         SPACE 2                                                                
DBNLDSCT DSECT                                                                  
DBNLWRK  DS    0CL14               LENGTH DISPLY REC                            
DBNLLMM  DS    CL2                 LENGTH                                       
DBNLPUN  DS    CL2                 PRIOR UNITS                                  
DBNLUN   DS    CL2                 UNITS                                        
DBNLPDL  DS    CL4                 PRIOR DOLLARS                                
DBNLDL   DS    CL4                 DOLLARS                                      
         SPACE 2                                                                
SVDSECT  DSECT                                                                  
*                                                                               
SVPGENTY DS    0CL12                                                            
SVPGREP  DS    CL2                 REP/AGENCY/USER POWER CODE                   
SVPGP#   DS    CL1                 SFM PROGRAM NUMBER (18)                      
         DS    CL1                                                              
SVPGPBIT DS    CL8                 64 PROFILES                                  
*                                                                               
*                                                                               
         EJECT                                                                  
       ++INCLUDE REGENAUR                                                       
         CSECT                                                                  
*                                                                               
*  SETAGY:  DEVELOP THE AGENCY KEY IF JUST AGENCY CODE, OR AGENCY/OFF           
*                                                                               
SETAGY   NMOD1 0,*SAGY*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R4,4(R1)            RESET R4 FOR ROUTINE                         
         LA    R2,22(R4)           A(AGENCY/AGENCY-OFFICE CODE)                 
         ZIC   RE,1(R4)            L(INPUT)                                     
         AR    R2,RE               FIND LAST POSITION OF FIELD                  
         BCTR  R2,0                BACK UP 1                                    
SETA0010 EQU   *                                                                
         CLI   0(R2),C'-'          SEPARATOR FOUND?                             
         BE    SETA0020            YES                                          
         BCTR  R2,0                NO  - BACK UP 1 CHARACTER                    
         BCT   RE,SETA0010         GO BACK FOR NEXT CHARACTER                   
         CLI   1(R4),4             NO '-' FOUND - CHECK LENGTH                  
         BH    SETA0090            TOO LONG - RETURN ERROR                      
         MVI   AGOFFFLG,C'C'       SET FLAG TO 'CODE ONLY'                      
         B     SETA0030                                                         
SETA0020 EQU   *                                                                
         BCTR  RE,0                CHECK LENGTH OF PRE-'-'                      
         CH    RE,=H'4'            MAX REMAINING = 4                            
         BH    SETA0090            TOO LONG - RETURN ERROR                      
         MVI   AGOFFFLG,C'O'       SET FLAG TO 'CODE+AGY OFFICE'                
SETA0030 EQU   *                                                                
         LA    R2,22(R4)           A(AGENCY/AGENCY-OFFICE CODE)                 
         ZIC   RE,1(R4)            L(INPUT)                                     
         LA    R1,KEY+19           A(KEY FIELD)                                 
SETA0040 EQU   *                                                                
         MVC   0(1,R1),0(R2)       MOVE AGENCY CODE TO KEY                      
         LA    R1,1(R1)            BUMP A(KEY)                                  
         LA    R2,1(R2)            BUMP A(SENDING)                              
         CLI   0(R2),C'-'          SEPARATOR?                                   
         BE    SETA0050            YES                                          
         BCT   RE,SETA0040         NO  - GO BACK FOR NEXT CHARACTER             
         B     SETA0070            FINISHED, NO SEPARATOR FOUND                 
SETA0050 EQU   *                                                                
         BCTR  RE,0                SKIP SEPARATOR                               
         LA    R2,1(R2)                                                         
         LA    R1,KEY+23           POINT TO KEY AGY/OFF FIELD                   
SETA0060 EQU   *                                                                
         MVC   0(1,R1),0(R2)       MOVE IN AGENCY OFFICE                        
         LA    R1,1(R1)            BUMP A(KEY)                                  
         LA    R2,1(R2)            BUMP A(SENDING)                              
         BCT   RE,SETA0060         GO BACK FOR NEXT CHARACTER                   
SETA0070 EQU   *                                                                
         SR    R0,R0               RETURN CC = ZERO:  OKAY                      
SETA0080 EQU   *                                                                
         XIT1                                                                   
SETA0090 EQU   *                                                                
         LTR   RB,RB               SET CC = NOT ZERO:  ERROR                    
         B     SETA0080                                                         
         EJECT                                                                  
         LTORG                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'069RESFM06   05/01/02'                                      
         END                                                                    
