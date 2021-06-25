*          DATA SET REWRI0052  AT LEVEL 053 AS OF 05/01/02                      
*PHASE T82100B,*                                                                
*INCLUDE UNBOOK                                                                 
*INCLUDE KHDUMMY                                                                
           TITLE 'T82100 - REPPAK WRITER CONTROLLER'                            
***********************************************************************         
*                                                                     *         
*          TITLE  T82100 - REPPAK WRITER CONTROLLER                   *         
*  COMMENTS: REPPAK WRITER CONTROLLER                                 *         
*                                                                     *         
*  LOCALS: REGISTER USAGE                                             *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR     *         
*          R3 - WORK REG                                              *         
*          R4 - WORK REG, KEY DSECT POINTER, GETEL REG                *         
*          R5 - WORK REG                                              *         
*          R6 - THIRD BASE                                            *         
*          R7 - SECOND BASE                                           *         
*          R8 - POINTER TO SPOOLD                                     *         
*          R9 - POINTER TO SYSD                                       *         
*          RA - POINTER TO ATWA                                       *         
*          RB - FIRST BASE                                            *         
*          RC - POINTER TO GEND                                       *         
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (REWRI00-T23000)         *         
*                  - IN AIO FROM HYPER CONTROLLER (T00A30)            *         
*             AIO2 -                                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
***********************************************************************         
*                                                                     *         
*  HISTORY OF CHANGES                                                 *         
*                                                                     *         
*---------------------------------------------------------------------*         
*                                                                     *         
*  22SEP98 (BOB) --- ADD TEST VERSION OF AUR                          *         
*  06AUG98 (BOB) --- ADD EXTENDED RFP OPTION                          *         
*  12MAR98 (EFJ) --- ADD DOWNAUR OPTION                               *         
*  24JUN97 (EFJ) --- CHANGE STREAM TO ACOST                           *         
*  23JUN97 (EFJ) --- DOWNHEAD OPTION                                  *         
*  06JUN97 (EFJ) --- READ CONTRACT PROFILE FROM REP REC               *         
*  04JUN97 (EFJ) --- SET ASPOOLD IN COMMON                            *         
*  03JUN97 (EFJ) --- NEW AUR STREAMLINED OPTION                       *         
*  24MAR97 (EFJ) --- ADD ADDRESSABILITY                               *         
*                --- INSTALL NOP'ED OPTION                            *         
*                --- FIX BOOK FILTER CODE                             *         
*                --- PRINT LENGTH FILTER                              *         
*  27FEB97 (EFJ) --- SUPPORT DEMROUND OPTION FOR AUR (L27)            *         
*                     (**NOP'ED - ADDRESSABILITY ERRORS)              *         
*                                                                     *         
*  12FEB97 (EFJ) --- CHANGE REPORT CODE FOR AUR TO AU (FROM OW)       *         
*  17APR96 (EFJ) --- SUPPORT LENGTH FILTER (AUR)   LEV 20             *         
*  02APR96 (EFJ) --- SUPPORT FOR AUR (LEVELS 10==>19)                 *         
*  08NOV95 (BU ) --- CHANGE REGENALL TO REGENALL1 (2K BUFFER/CONTRACT)*         
*  04DEC96 (BG ) --- USE PERIOD DATES IF ACT DATES NOT ENTERED        *         
*                                                                     *         
*                                                                     *         
*                    ***  END TOMBSTONE  ***                          *         
***********************************************************************         
         EJECT                                                                  
T82100   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WKEND-WKST,T82100,R6,R7,RR=R2,CLEAR=YES                          
         ST    R2,RELO                                                          
         LR    R9,R1                                                            
         LR    R8,RC                                                            
         USING SPOOLD,R8                                                        
         LA    RC,SPOOLEND                                                      
         USING GEND,RC                                                          
         ST    R9,SYSPARMS                                                      
         LA    R9,IO                                                            
         AH    R9,=H'2000'         GRABBING 2 1000 BYTE I/O AREAS               
         LA    R9,16(R9)           NEED SPACE FOR 2 8 BYTE LABELS               
         ST    R9,ASUBSYSD                                                      
         USING SUBSYSD,R9                                                       
         L     R1,SYSPARMS                                                      
         L     RA,4(R1)                                                         
         USING T821FFD,RA                                                       
         SPACE                                                                  
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(131),SPACES                                             
         BAS   RE,SYSINIT          INITIALIZE SYSTEM DEPENDENT                  
         OI    GENSTAT5,USERFPXT   USE EXTENDED SYMBOL TABLE FOR RFP            
         NI    CONKEYH+1,X'FF'-X'20'  UNPROTECT ACTION FOR GENCON               
         OI    CONSERVH+1,X'01'     SERVICE FIELD IS ALWAYS MODIFIED            
         OI    CONSERVH+6,X'80'                                                 
         ST    RB,SYSRB            SYSTEM BASE REGS                             
         ST    RD,SYSRD                                                         
         SPACE                                                                  
         GOTO1 GENCON,DMCB,(R8)    OFF TO GENCON                                
         B     XIT                 THEN WE'RE THROUGH                           
         SPACE 3                                                                
* SYSTEM ROUTINES ENTERABLE FROM BASE OR OVERLAY *                              
*                                                                               
VCOMMON  NTR1  BASE=SYSRB                                                       
         ST    RD,COMMRD                                                        
         L     R9,ASYSD                                                         
         USING SUBSYSD,R9                                                       
         LM    R6,R7,BASER6                                                     
         L     RA,ATWA                                                          
         USING T821FFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         SRL   RF,24                                                            
         CLM   RF,1,=AL1((BRMAX-BRANCH)/4)                                      
         BNH   *+6                                                              
         DC    H'0'                                                             
         SLL   RF,2                                                             
         B     BRANCH(RF)                                                       
         SPACE                                                                  
BRANCH   DS    0H                                                               
         EJECT                                                                  
         B     VUSERVAL            VALIDATE USER ID                             
         B     VREG                         REGION                              
         B     VOFF                VALIDATE OFFICE                              
         B     VPDT                         PERIOD DATES - FLIGHT               
         B     VGRPSUB                      GROUP/SUBGRP                        
         B     VSTA                         STATION CALL LETTERS                
         B     VACT                         ACTIVITY DATE                       
         B     VSAL                VALIDATE SALESPERSON                         
         B     VDT                          DIVISION/TEAM                       
         B     VADV                         ADVERTISER                          
         B     VAGY                         AGENCY                              
         B     VCLS                         CLASS                               
         B     VCAT                         CATEGORY                            
         B     VPRD                         PRODUCT                             
         B     VBOK                         BOOK                                
*        B     VTRACE              TRACE DATA BLOCK                             
         DC    4X'00'                                                           
         SPACE                                                                  
         B     VWRIUSER                                                         
         B     VVALFILT                     FILTERS                             
         B     VVALOPTS                     OPTIONS                             
         B     VVALTITS                                                         
         DC    12X'00'                                                          
         SPACE                                                                  
         B     VVALLEFT                                                         
         B     VVALRGHT                                                         
         B     VVALMID                                                          
         B     VVALROWS                                                         
         B     VVALCOLS                                                         
         DC    12X'00'                                                          
         SPACE                                                                  
         B     VINTDRIV                                                         
         B     VINTDRON                                                         
         B     VWRPDRON                                                         
         B     VINTHEAD                                                         
         DC    8X'00'                                                           
         SPACE                                                                  
         B     VGENHEAD                                                         
         DS    12X'00'                                                          
         SPACE                                                                  
         B     VNUMERIC                                                         
         B     VPACK                                                            
         DC    12X'00'                                                          
         SPACE                                                                  
         B     VCURSERR                                                         
         B     VERRXIT                                                          
         DC    12X'00'                                                          
         SPACE                                                                  
BRMAX    EQU   *                                                                
         SPACE                                                                  
         LTORG                                                                  
         EJECT                                                                  
VUSERVAL DS    0H                  VALIDATE USER ID                             
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RREPKEY,R4                                                       
         MVI   RREPKTYP,RREPKTYQ                                                
         MVC   RREPKREP,AGENCY                                                  
         GOTO1 HIGH                                                             
         CLC   RREPKEY,KEYSAVE                                                  
         BNE   REPERR                                                           
         SPACE                                                                  
         L     R4,AIO1                                                          
         ST    R4,AIO                                                           
         GOTO1 GETREC                                                           
         MVI   ELCODE,01                                                        
         BAS   RE,GETEL                                                         
         BNE   REPERR                                                           
         SPACE                                                                  
         USING RREPELEM,R4                                                      
         MVC   USERNAME,RREPNAME                                                
         MVC   USERADDR,RREPADDR                                                
*                                                                               
         L     R4,AIO1                                                          
         MVI   ELCODE,X'04'                                                     
         USING RREPPGMP,R4                                                      
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         ZIC   RF,RREPPGM#         # OF PROGRAM UNITS (LOOP COUNTER)            
         LA    R4,RREPPGM1                                                      
         USING RREPPGM1,R4                                                      
VUSERV10 CLI   RREPPGM1,RREPQCNT   CONTRACT?                                    
         BE    VUSERV20                                                         
         LA    R4,RREPPGML(R4)                                                  
         BCT   RF,VUSERV10                                                      
         B     XIT                 CONTRACT NOT FOUND. USE DEFAULTS.            
*                                                                               
VUSERV20 MVC   PROFILES,RREPPGM1+2  SAVE PROGRAM PROFILES UNIT                  
         B     XIT                                                              
         DROP  R4                                                               
         SPACE 3                                                                
*                                                                               
*- VALIDATE REGION                                                              
*                                                                               
VREG     XC    REFREG,REFREG                                                    
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RREGKEY,R4                                                       
         MVI   RREGKTYP,RREGKTYQ                                                
         MVC   RREGKREP,AGENCY                                                  
         MVC   RREGKREG,8(R2)                                                   
         GOTO1 HIGH                                                             
         CLC   RREGKEY,KEYSAVE                                                  
         BNE   REGERR                                                           
         MVC   REFREG,8(R2)                                                     
         OI    REQTABLE,REQOFRG                                                 
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*- VALIDATE OFFICE                                                              
*                                                                               
VOFF     DS    0H                                                               
         XC    REFOFF,REFOFF                                                    
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING ROFFKEY,R4                                                       
         MVI   ROFFKTYP,ROFFKTYQ                                                
         MVC   ROFFKREP,AGENCY                                                  
         MVC   ROFFKOFF,8(R2)                                                   
         OC    ROFFKOFF(2),SPACES    CONVERT X'00' TO BLANKS                    
         GOTO1 HIGH                                                             
         CLC   ROFFKEY,KEYSAVE                                                  
         BNE   OFFERR                                                           
         SPACE                                                                  
         CLI   OFFLINE,C'Y'        IF OFFLINE SECURITY CK DONE ONLINE           
         BE    VOFF10                                                           
         SPACE                                                                  
*- OUTSIDE OFFICE FOR A REP?                                                    
         SPACE                                                                  
         CLC   =C'O=',TWAACCS      TEST FOR OFFICE LIMITED ACCESS               
         BNE   VOFF10                                                           
         SPACE                                                                  
         CLI   TWAOFFC,C'*'        DDS TERMINAL                                 
         BE    VOFF10               YES                                         
         SPACE                                                                  
         TM    TWAAUTH,X'80'       TERMINAL HAS ACCESS TO ALL OFFICES?          
         BO    VOFF10               YES                                         
         SPACE                                                                  
         CLC   8(2,R2),TWAACCS+2   NO, MUST MATCH RESTRICTED OFFICE             
         BC    0,VOFFSERR            NOPPED AT KARI'S REQUEST 12/10/90          
VOFF10   MVC   REFOFF,8(R2)                                                     
         OC    REFOFF(2),SPACES    CONVERT X'00' TO BLANKS                      
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              VALIDATE FLIGHT DATE FIELDS                                      
         SPACE                                                                  
VPDT     XC    REQPDTS,REQPDTS                                                  
         XC    WORK,WORK                                                        
         XC    AACTFLD,AACTFLD     ZERO ACTIVITY DATES FIELD                    
         SPACE                                                                  
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         ST    R2,APERFLD                                                       
         SPACE                                                                  
         LA    R3,8(R2)                                                         
         GOTO1 DATVAL,DMCB,(0,(R3)),WORK    TRY FOR MMMDD/YY                    
         OC    DMCB(4),DMCB                                                     
         BNZ   PDTERR                                                           
         GOTO1 (RF),(R1),(2,(R3)),REQPSTR   THEN MMM/YY                         
         ICM   R0,15,DMCB                                                       
         BZ    PDTERR                                                           
         SPACE                                                                  
         GOTO1 DATCON,(R1),REQPSTR,(6,REQPSTRM)                                 
         MVC   REQPENDM,REQPSTRM                                                
         SPACE                                                                  
         AR    R3,R0                                                            
         MVC   REQPSTR+4(2),=C'15'                                              
         GOTO1 QGTBROAD,(R1),(1,REQPSTR),WORK,GETDAY,ADDAY                      
         SPACE                                                                  
         CLI   DMCB,X'FF'          DATE ERROR                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         MVC   REQPSTR(12),WORK                                                 
         CLI   0(R3),C' '                                                       
         BNH   VPDT50                                                           
         CLI   0(R3),C'-'          - AND , ARE ACCEPTABLE DELIMITERS            
         BE    VPDT40                                                           
         CLI   0(R3),C','                                                       
         BE    VPDT40                                                           
         B     PDTERR                                                           
         SPACE                                                                  
VPDT40   LA    R3,1(R3)                                                         
         GOTO1 DATVAL,(R1),(0,(R3)),WORK    TRY FOR MMMDD/YY                    
         OC    DMCB(4),DMCB                                                     
         BNZ   PDTERR                                                           
         GOTO1 (RF),(R1),(2,(R3)),REQPEND   THEN MMM/YY                         
         ICM   RE,15,DMCB                                                       
         BZ    PDTERR                                                           
         SPACE                                                                  
         GOTO1 DATCON,(R1),REQPEND,(6,REQPENDM)                                 
         SPACE                                                                  
         OI    REQFLAGS,REQ2MON    SET ON 2 MONTH OR MORE REQUEST               
         MVC   REQPEND+4(2),=C'15'                                              
         GOTO1 QGTBROAD,(R1),(1,REQPEND),WORK,GETDAY,ADDAY                      
         CLI   DMCB,X'FF'          DATE ERROR                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   REQPEND,WORK+6                                                   
         SPACE                                                                  
VPDT50   GOTO1 DATCON,(R1),(0,REQPSTR),(3,REQPSTRB)                             
         GOTO1 (RF),(R1),(0,REQPEND),(3,REQPENDB)                               
         CLC   REQPEND,REQPSTR                                                  
         BL    PDTERR                                                           
         MVC   USERQSTR,REQPSTR                                                 
         MVC   USERQEND,REQPEND                                                 
         B     XIT                                                              
         EJECT                                                                  
*                                  VALIDATE GROUP/SUBGRP                        
         SPACE                                                                  
VGRPSUB  XC    REFGRPSB,REFGRPSB                                                
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RGRPKEY,R4                                                       
         MVI   RGRPKTYP,RGRPKTYQ                                                
         MVC   RGRPKREP,AGENCY                                                  
         MVC   RGRPKGRP,8(R2)                                                   
         SPACE                                                                  
         GOTO1 HIGH                                                             
         CLI   5(R2),1             WAS ONLY GROUP ENTERED                       
         BE    VGRPS10                                                          
         CLC   RGRPKEY,KEYSAVE                                                  
         BNE   GRPERR                                                           
         MVC   REFGRPSB,8(R2)                                                   
         B     XIT                                                              
         SPACE                                                                  
VGRPS10  CLC   RGRPKEY(26),KEYSAVE ONLY NEED GROUP                              
         BNE   GRPERR                                                           
         MVC   REFGRP,8(R2)                                                     
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                  VALIDATE STATION CALL LETTERS                
         SPACE                                                                  
*                                                                               
*- STAKEY -- PARSE STATION FIELD INPUT (6 CHARS)                                
*            AND BUILD KEY FOR FILE READING.                                    
*                                                                               
VSTA     XC    REFSTA,REFSTA                                                    
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         MVC   REFSTA,SPACES                                                    
         ZIC   R0,5(R2)            PARSE OUT POSSIBLE '-'                       
         LA    RF,8(R2)                                                         
         LA    RE,REFSTA                                                        
VSTA20   CLI   0(RF),C'-'                                                       
         BNE   VSTA40                                                           
*                                                                               
         LA    RF,1(RF)            SKIP THE '-'                                 
         LA    RE,REFSTA+4         BAND MUST GO HERE                            
         BCTR  R0,0                LESS 1 ON IPT LEN (THE '-')                  
         LTR   R0,R0                                                            
         BZ    VSTA60            NOTHING AFTER THE '-' (WABC- )                 
         LA    R0,1                OUT OF LOOP AFTER NEXT MOVE.                 
*                                                                               
VSTA40   MVC   0(1,RE),0(RF)       MOVE DATA TO KEY                             
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,VSTA20                                                        
*                                                                               
*- IF BAND = 'T' (TELEVISION) THEN BLANK OUT BAND IN KEY.                       
         SPACE                                                                  
         CLI   REFSTA+4,C'T'                                                    
         BNE   *+8                                                              
         MVI   REFSTA+4,C' '                                                    
         SPACE                                                                  
VSTA60   DS    0H                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RSTAKEY,R4                                                       
         MVI   RSTAKTYP,RSTAKTYQ                                                
         MVC   RSTAKREP,AGENCY                                                  
         MVC   RSTAKSTA,REFSTA                                                  
         GOTO1 HIGH                                                             
         CLC   RSTAKEY,KEYSAVE                                                  
         BNE   STAERR                                                           
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                  ACTIVITY DATE VALIDATION                     
         SPACE                                                                  
VACT     XC    REQADTS,REQADTS                                                  
         XC    AACTFLD,AACTFLD                                                  
         XC    WORK,WORK                                                        
         SPACE                                                                  
         CLC   =C'A=',8(R2)        IS IT CONTRACT ADD DATES?                    
         BNE   VACT09                                                           
************************************************************                    
         ZIC   R4,5(R2)          GET LENGTH OF INPUT                            
         BCTR  R4,0              DROP "A="                                      
         BCTR  R4,0                                                             
         PRINT GEN                                                              
         GOTO1 PERVAL,DMCB,((R4),10(R2)),WORK                                   
         PRINT NOGEN                                                            
         OC    4(1,R1),4(R1)         BOTH DATES OK ?                            
         BZ    VACT07                                                           
         TM    4(R1),X'04'         SINGLE DATE ONLY ?                           
         BO    VACT07              OK                                           
         B     ACTERR              PROBLEM !                                    
VACT07   LA    R1,WORK                                                          
         USING PERVALD,R1                                                       
         MVC   REQADDST,PVALBSTA           START                                
         MVC   REQADDED,PVALBSTA           START AS END                         
         OC    PVALBEND,PVALBEND                                                
         BZ    *+10                                                             
         MVC   REQADDED,PVALBEND            END                                 
* WHEN USING CONT ADD DAY - MUST HAVE PERIOD DATES                              
         OC    REQPDTS,REQPDTS     PERIOD DATES?                                
         BZ    PDTERR              NO                                           
* IF NOT A= AND NO ACTIVITY DATE, VACT09 BELOW USES PERIOD DATES                
*                                 AS ACTIVITY DATES                             
*                                                                               
         MVC   REQADTS(12),REQPDTS  USE PERIOD DATES AS ACTIVITY DATS           
         B     VACT70               FILL IN REST OF DATES REQUIRED              
         DROP  R1                                                               
***************************************************************                 
VACT09   CLI   5(R2),0             SEE IF ENTRY                                 
         BNE   VACT10               YES                                         
         SPACE                                                                  
         MVC   REQADTS(12),REQPDTS  USE PERIOD DATES                            
         B     VACT60               FILL IN REST OF DATES REQUIRED              
         SPACE                                                                  
VACT10   ST    R2,AACTFLD                                                       
         MVI   REQAPER,C'N'                                                     
         SPACE                                                                  
         LA    R3,8(R2)                                                         
         GOTO1 DATVAL,DMCB,(0,(R3)),REQASTR TRY FOR MMMDD/YY                    
         ICM   R0,15,DMCB                                                       
         BZ    ACTERR                                                           
         MVC   REQAEND,REQASTR                                                  
         SPACE                                                                  
         AR    R3,R0                                                            
         CLI   0(R3),C' '                                                       
         BNH   VACT60                                                           
         CLI   0(R3),C'-'          - AND , ARE ACCEPTABLE DELIMITERS            
         BE    VACT40                                                           
         CLI   0(R3),C','                                                       
         BNE   ACTERR                                                           
         SPACE                                                                  
VACT40   LA    R3,1(R3)                                                         
         GOTO1 DATVAL,(R1),(0,(R3)),REQAEND  TRY FOR MMMDD/YY                   
         OC    DMCB,DMCB                                                        
         BZ    ACTERR                                                           
         SPACE                                                                  
VACT60   CLC   REQAEND,REQASTR                                                  
         BL    ACTERR                                                           
         MVI   BYTE,0                                                           
         GOTO1 GETDAY,(R1),REQASTR,WORK                                         
         CLC   WORK(3),SPACES                                                   
***      BNE   *+6                                                              
***      DC    H'0'                                                             
         BE    ACTERR              WHY CRASH - GIVE EM ERROR                    
         CLI   DMCB,1              IF MONDAY, OK                                
         BE    VACT64                                                           
         MVI   BYTE,1                                                           
         ZIC   R0,DMCB                                                          
         BCTR  R0,0                                                             
         LNR   R0,R0                                                            
         GOTO1 ADDAY,DMCB,REQASTR,REQASTR,(R0)                                  
         SPACE                                                                  
VACT64   GOTO1 GETDAY,(R1),REQAEND,WORK                                         
         CLC   WORK(3),SPACES                                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   DMCB,7              IF SUNDAY, OK                                
         BE    VACT66                                                           
         MVI   BYTE,1                                                           
         ZIC   R0,DMCB                                                          
         SH    R0,=H'7'                                                         
         LCR   R0,R0                                                            
         GOTO1 ADDAY,DMCB,REQAEND,REQAEND,(R0)                                  
         SPACE                                                                  
VACT66   CLI   BYTE,0                                                           
         BE    VACT70                                                           
         GOTO1 DATCON,DMCB,REQASTR,(5,8(R2))                                    
         MVI   16(R2),C'-'                                                      
         GOTO1 (RF),(R1),REQAEND,(5,17(R2))                                     
         MVI   5(R2),17            SET NEW LENGTH                               
         OI    6(R2),X'80'         FORCE TRANSMIT                               
         SPACE                                                                  
VACT70   MVC   REQAASTR,REQASTR                                                 
         MVC   REQAAEND,REQAEND                                                 
         GOTO1 PERVERT,(R1),REQASTR,REQAEND                                     
         MVC   REQAAWKS,DMCB+13    SAVE WEEKS IN PERIOD                         
         B     XIT                                                              
         EJECT                                                                  
*                                  VALIDATE SALESPERSON                         
         SPACE                                                                  
VSAL     XC    REFSAL,REFSAL                                                    
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RSALKEY,R4                                                       
         MVI   RSALKTYP,RSALKTYQ                                                
         MVC   RSALKREP,AGENCY                                                  
         MVC   RSALKSAL,8(R2)                                                   
         OC    RSALKSAL,SPACES                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   SALERR                                                           
         SPACE                                                                  
         OC    REFOFF,REFOFF       WAS OFFICE ENTERED                           
         BZ    VSAL10                                                           
         L     R4,AIO1                                                          
         ST    R4,AIO                                                           
         GOTO1 GETREC                                                           
         MVI   ELCODE,01                                                        
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RSALELEM,R4                                                      
         CLC   RSALOFF,REFOFF      SALESMAN IN OFFICE                           
         BNE   SALOFFER                                                         
         DROP  R4                                                               
VSAL10   MVC   REFSAL,8(R2)                                                     
         OC    REFSAL,SPACES                                                    
         B     XIT                                                              
         EJECT                                                                  
*                                  VALIDATE DIVISION/TEAM                       
         SPACE                                                                  
VDT      XC    REFDT,REFDT                                                      
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RTEMKEY,R4                                                       
         MVI   RTEMKTYP,RTEMKTYQ                                                
         MVC   RTEMKREP,AGENCY                                                  
         MVC   RTEMKTEM,8(R2)                                                   
         GOTO1 HIGH                                                             
         CLI   5(R2),1             WAS ONLY GROUP ENTERED                       
         BE    VDT10                                                            
         CLC   RTEMKEY,KEYSAVE                                                  
         BNE   DTERR                                                            
         MVC   REFDT,8(R2)                                                      
         B     XIT                                                              
         SPACE                                                                  
VDT10    CLC   RTEMKEY(26),KEYSAVE ONLY NEED DIVISION                           
         BNE   DTERR                                                            
         MVC   REDIV,8(R2)                                                      
         B     XIT                                                              
         DROP  R4                                                               
         SPACE 3                                                                
*                                  VALIDATE ADVERTISER                          
         SPACE                                                                  
VADV     XC    REFADV,REFADV                                                    
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RADVKEY,R4                                                       
         MVI   RADVKTYP,RADVKTYQ                                                
         MVC   RADVKREP,AGENCY                                                  
         MVC   RADVKADV,8(R2)                                                   
         OC    RADVKADV,SPACES                                                  
         GOTO1 HIGH                                                             
         CLC   RADVKEY,KEYSAVE                                                  
         BNE   ADVERR                                                           
         MVC   REFADV,8(R2)                                                     
         OC    REFADV,SPACES                                                    
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                  VALIDATE AGENCY (& AOF)                      
         SPACE                                                                  
* MAY NEED TO CHECK OFFICE CODE IN KEY                                          
         SPACE                                                                  
VAGY     XC    REFAGY,REFAGY                                                    
         XC    REFAOF,REFAOF                                                    
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         MVC   REFAGY,SPACES                                                    
         MVC   REFAOF,SPACES                                                    
         LA    R0,5                                                             
         LA    R1,8(,R2)                                                        
         LA    RE,REFAGY                                                        
         SR    RF,RF                                                            
VAGY10   CLI   0(R1),C'-'                                                       
         BE    VAGY14                                                           
         CLI   0(R1),C' '                                                       
         BNH   VAGY14                                                           
         MVC   0(1,RE),0(R1)                                                    
         LA    R1,1(,R1)                                                        
         LA    RE,1(,RE)                                                        
         LA    RF,1(,RF)                                                        
         BCT   R0,VAGY10                                                        
         B     OFFLENER                                                         
         SPACE                                                                  
VAGY14   CLM   RE,1,5(R2)          THIS ALL THERE IS                            
         BE    VAGY20                                                           
         LA    R0,2                                                             
         LA    R1,1(,R1)                                                        
         LA    RE,REFAOF                                                        
         SR    RF,RF                                                            
VAGY16   CLI   0(R1),C' '                                                       
         BNH   VAGY18                                                           
         MVC   0(1,RE),0(R1)                                                    
         LA    R1,1(,R1)                                                        
         LA    RE,1(,RE)                                                        
         LA    RF,1(,RF)                                                        
         BCT   R0,VAGY16                                                        
VAGY18   CH    RF,=H'2'                                                         
         BH    AOFLENER                                                         
         SPACE                                                                  
VAGY20   XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RAGYKEY,R4                                                       
         MVI   RAGYKTYP,RAGYKTYQ                                                
         MVC   RAGYKAGY,REFAGY                                                  
         MVC   RAGYKAOF,REFAOF                                                  
         MVC   RAGYKREP,AGENCY                                                  
         GOTO1 HIGH                                                             
         CLC   RAGYKEY,KEYSAVE                                                  
         BNE   AGYERR                                                           
         SPACE                                                                  
         CLC   REFAOF,SPACES                                                    
         BNE   XIT                                                              
         XC    REFAOF,REFAOF                                                    
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                  VALIDATE CLASS                               
         SPACE                                                                  
VCLS     XC    REFCLS,REFCLS                                                    
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         SPACE                                                                  
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RCLSKEY,R4                                                       
         MVI   RCLSKTYP,RCLSKTYQ                                                
         MVC   RCLSKREP,AGENCY                                                  
         MVC   RCLSKCLS,8(R2)                                                   
         OC    RCLSKCLS,SPACES                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   CLSERR                                                           
         SPACE                                                                  
         MVC   REFCLS,8(R2)                                                     
         OC    REFCLS,SPACES                                                    
         OI    REQTABLE,REQCTCL                                                 
         B     XIT                                                              
         DROP  R4                                                               
         SPACE                                                                  
*                                  VALIDATE CATEGORY                            
         SPACE                                                                  
VCAT     XC    REFCTG,REFCTG                                                    
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RCTGKEY,R4                                                       
         MVI   RCTGKTYP,RCTGKTYQ                                                
         MVC   RCTGKREP,AGENCY                                                  
         MVC   RCTGKCTG,8(R2)                                                   
         OC    RCTGKCTG,SPACES                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   CATERR                                                           
         SPACE                                                                  
         MVC   REFCTG,8(R2)                                                     
         OC    REFCTG,SPACES                                                    
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                  VALIDATE PRODUCT                             
         SPACE                                                                  
VPRD     XC    REFPRD,REFPRD                                                    
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         SPACE                                                                  
         CLI   REFADV,0            WAS ADVERTISER ENTERED                       
         BE    MISADVER             NO, ERROR                                   
         SPACE                                                                  
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RPRDKEY,R4                                                       
         MVI   RPRDKTYP,RPRDKTYQ                                                
         MVC   RPRDKADV,REFADV                                                  
         MVC   RPRDKPRD,8(R2)                                                   
         MVC   RPRDKREP,AGENCY                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   PRDERR                                                           
         SPACE                                                                  
         MVC   REFPRD,8(R2)                                                     
         B     XIT                                                              
         DROP  R4                                                               
         SPACE 2                                                                
*  VALIDATE BOOK(S)                                                             
         SPACE                                                                  
*     CREATE 4 BYTE BOOK FIELD FROM:                                            
*            3 BYTE BOOK IN WORK                                                
*            1 BYTE BOOKTYPE IN BKTYKPE                                         
         SPACE 2                                                                
VBOK     XC    REFBOOK,REFBOOK                                                  
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         SPACE                                                                  
         GOTO1 BOOKVAL,DMCB,(0,(R2)),(11,WORK),(C'B',SCANNER),BYTE              
         SPACE                                                                  
         CLI   4(R1),0                                                          
         BE    INVBOKER                                                         
         CLI   4(R1),1             ONLY 1 BOOK ALLOWED                          
         BH    MANYBKER                                                         
         SPACE                                                                  
         MVC   REFBOOK(3),WORK                                                  
         MVC   REFBOOK+3(1),BYTE                                                
         B     XIT                                                              
         EJECT                                                                  
         SPACE 3                                                                
VTRACE   DS    0H                  TRACE DATA BLOCK                             
         SPACE 3                                                                
         EJECT                                                                  
*              USER ID RECORD                                                   
         SPACE 2                                                                
VWRIUSER L     R1,SYSPARMS                                                      
*        MVC   AGENCY,0(R1)                                                     
         MVC   AGYSIGN,SPACES                                                   
         XC    AGYALPHA,AGYALPHA                                                
*                                  AGENCY NAME & ADDRESS FROM ID REC.           
         MVC   FILENAME,=CL8'CTFILE'                                            
         MVI   USEIO,C'Y'                                                       
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTIKEY,R4                                                        
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),10(RA)                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         L     R4,AIO                                                           
         LA    R4,CTIDATA                                                       
         SR    R3,R3                                                            
         SPACE                                                                  
VUSER10  CLI   0(R4),0                                                          
         BE    VUSER30                                                          
         CLI   0(R4),X'02'                                                      
         BE    VUSER24                                                          
         CLI   0(R4),X'06'                                                      
         BE    VUSER26                                                          
         CLI   0(R4),X'21'                                                      
         BE    VUSER28                                                          
         SPACE                                                                  
VUSER16  IC    R3,1(R4)                                                         
         AR    R4,R3                                                            
         B     VUSER10                                                          
         SPACE                                                                  
         USING CTDSCD,R4                                                        
VUSER24  MVC   AGYSIGN,CTDSC                                                    
         B     VUSER16                                                          
         SPACE                                                                  
         USING CTAGYD,R4                                                        
VUSER26  MVC   AGYALPHA,CTAGYID                                                 
         B     VUSER16                                                          
         SPACE                                                                  
         USING CTSYSD,R4                                                        
VUSER28  CLI   CTSYSNUM,8          THIS ONE'S FOR REPPAK                        
         BNE   VUSER16                                                          
         OC    TWAACCS(2),TWAACCS  ACCESS                                       
         BNZ   *+10                                                             
         MVC   TWAACCS,CTSYSLMT                                                 
         B     VUSER16                                                          
         SPACE                                                                  
VUSER30  XC    FILENAME,FILENAME                                                
         MVI   USEIO,0                                                          
         LA    R4,FACTWRK                                                       
         GOTO1 GETFACT,DMCB,(R4)                                                
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE FILTERS                                                 
         SPACE 3                                                                
VVALFILT DS    0H                  PRECLEAR FILTERS                             
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         GOTO1 SCANNER,DMCB,(20,(R2)),(6,BLOCK)                                 
         MVI   FIELDERR,1                                                       
         ZIC   R0,4(R1)                                                         
         LA    R4,BLOCK                                                         
         LTR   R0,R0                                                            
         BZ    BADFILT                                                          
         SPACE                                                                  
FILT2    ZIC   R1,0(R4)            L'FILTER EXPRESSION                          
         BCTR  R1,0                                                             
         LA    R3,FILTTAB                                                       
         SPACE                                                                  
FILT4    EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R3),12(R4)                                                   
         BE    FILTSET                                                          
         LA    R3,L'FILTENT(R3)                                                 
         CLI   0(R3),X'FF'                                                      
         BNE   FILT4                                                            
         SPACE                                                                  
BADFILT  MVC   CONHEAD(L'FTRERMS),FTRERMS                                       
         B     MYCURSOR                                                         
         SPACE                                                                  
FILTSET  SR    RE,RE               RELATIVE DISPLACEMENT IN FILTERS             
         ICM   RE,3,8(R3)          (NOW HAVE THE ADDRESS)                       
         AR    RE,RB               RB=BASE                                      
         BR    RE                                                               
         SPACE                                                                  
FILTEND  DS    0H                                                               
         SPACE                                                                  
FILTENDX LA    R4,42(,R4)                                                       
         AI    FIELDERR,1                                                       
         BCT   R0,FILT2                                                         
         B     XIT                                                              
         EJECT                                                                  
VSTO     DS    0H                  VALIDATE STATION OWNER                       
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING ROWNKEY,R3                                                       
         MVI   ROWNKTYP,ROWNKTYQ                                                
         MVC   ROWNKREP,AGENCY                                                  
         MVC   ROWNKOWN,22(R4)                                                  
         OC    ROWNKOWN,SPACES                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   STOERR                                                           
         MVC   REFOWN,22(R4)                                                    
         OC    REFOWN,SPACES                                                    
         OI    REQTABLE,REQSTOW    REQUEST STATION OWNER & RANK TABLE           
         B     FILTEND                                                          
         DROP  R3                                                               
         SPACE 3                                                                
* CONTRACT TYPE - AN ASTERISK PRECEDING MEANS EXCLUDE THIS CONTR TYPE *         
         SPACE                                                                  
VCTY     XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING RCTYKEY,R3                                                       
         MVI   RCTYKTYP,RCTYKTYQ                                                
         MVC   RCTYKREP,AGENCY                                                  
         MVC   RCTYKCTY,22(R4)                                                  
         CLI   22(R4),C'*'         MEANS EXCLUDE THIS CONTRACT TYPE             
         BNE   *+10                                                             
         MVC   RCTYKCTY,23(R4)                                                  
         SPACE                                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   CTYPERR                                                          
         MVC   REFCTY,22(R4)                                                    
         CLI   22(R4),C'*'         MEANS EXCLUDE THIS CONTRACT TYPE             
         BNE   FILTEND                                                          
         MVI   REFCTYEX,C'*'                                                    
         MVC   REFCTY,23(R4)                                                    
         B     FILTEND                                                          
         DROP  R3                                                               
         EJECT                                                                  
* VALIDATE RANK                                                                 
         SPACE                                                                  
VRNK     CLI   9(R4),1             MAX 1 CHAR                                   
         BH    RNKERR                                                           
         CLI   22(R4),C'0'                                                      
         BL    RNKERR                                                           
         CLI   22(R4),C'9'                                                      
         BH    RNKERR                                                           
         MVC   REFRNK,22(R4)                                                    
         OI    REQTABLE,REQSTOW    REQUEST STATION OWNER & RANK TABLE           
         B     FILTEND                                                          
         SPACE 3                                                                
* VALIDATE STATION TVB/REGION                                                   
         SPACE                                                                  
VTVB     L     R1,REATVBLS                                                      
VTVB10   CLC   22(2,R4),0(R1)                                                   
         BE    VTVB20                                                           
         LA    R1,20(,R1)                                                       
         CLI   0(R1),X'FF'                                                      
         BNE   VTVB10                                                           
         B     VTVBERR                                                          
         SPACE                                                                  
VTVB20   MVC   REFTVB,22(R4)                                                    
         B     FILTEND                                                          
         EJECT                                                                  
* VALIDATE DEMO(S)                                                              
         SPACE                                                                  
*  FOR ROUTINES WITH OPTION OF TP OR PAV FILE, THAT FIELD MUST BE               
*  VALIDATED AND SVFILE SET BEFORE COMING TO THIS ROUTINE                       
         SPACE 2                                                                
VDEM     ZIC   R1,1(R4)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),22(R4)                                                   
         SPACE                                                                  
         LA    R5,ELEM                                                          
         USING DBLOCKD,R5                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'T'                                                    
         MVC   DBFILE,=C'PAV'                                                   
         GOTO1 DEMOVAL,PARAS,(1,WORK),(1,REFDEMO),(0,(R5))                      
         CLI   4(R1),0                                                          
         BE    INVDEMER                                                         
         SPACE                                                                  
         MVC   REFDEMO,22(R4)                                                   
         B     FILTEND                                                          
         DROP  R5                                                               
         EJECT                                                                  
*                                  VALIDATE STATION TYPE                        
         SPACE                                                                  
VSTY     XC    REFSTAT,REFSTAT                                                  
         SPACE                                                                  
         CLI   22(R4),C'O'         OLD                                          
         BE    *+12                                                             
         CLI   22(R4),C'N'         NEW                                          
         BNE   STYERR                                                           
         SPACE                                                                  
         MVC   REFSTAT,22(R4)                                                   
         OC    REFSTAT,SPACES                                                   
         B     FILTEND                                                          
         SPACE                                                                  
*                                  VALIDATE LENGTH                              
VLEN     XC    REFLEN,REFLEN                                                    
         MVC   REFLENE,22(R4)      SAVE EBCDIC LENGTH                           
         TM    3(R4),X'80'         NUMERIC?                                     
         BZ    LEN50                                                            
         CLC   8(4,R4),=X'00007FFF'                                             
         BH    LENERR                                                           
         MVC   REFLEN,10(R4)       SAVE VALUE                                   
         B     FILTEND                                                          
*                                                                               
LEN50    LA    RF,4                                                             
         LA    R1,22(R4)                                                        
*                                                                               
LEN60    CLI   0(R1),C'M'          MINUTES?                                     
         BE    LEN100                                                           
         CLI   0(R1),X'F0'                                                      
         BL    LENERR                                                           
         CLI   0(R1),X'F9'                                                      
         BH    LENERR                                                           
         LA    R1,1(R1)                                                         
         BCT   RF,LEN60                                                         
         B     LENERR                                                           
*                                                                               
LEN100   DS    0H                                                               
         LA    R1,4                                                             
         SR    R1,RF                                                            
         BNP   LENERR                                                           
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,22(0,R4)                                                     
         CVB   RF,DUB                                                           
         STCM  RF,3,REFLEN                                                      
         OI    REFLEN,X'80'        MINUTES IND                                  
         B     FILTEND                                                          
         EJECT                                                                  
*              TABLE OF FILTER EXPRESSIONS                                      
         SPACE 3                                                                
FILTTAB  DS    0H                                                               
FILTENT  DS    0CL10                                                            
*                FILTER        RTN                                              
         DC    C'CONTYPE ',AL2(VCTY-T82100)                                     
         DC    C'RANK    ',AL2(VRNK-T82100)                                     
         DC    C'OWNER   ',AL2(VSTO-T82100)                                     
         DC    C'TVB/REG ',AL2(VTVB-T82100)                                     
         DC    C'STATYPE ',AL2(VSTY-T82100)                                     
         DC    C'LENGTH  ',AL2(VLEN-T82100)                                     
         DC    C'DEMO    ',AL2(VDEM-T82100)                                     
         DC    C'HELP    ',AL2(FHELP-T82100)                                    
         DC    X'FF'                                                            
       ++INCLUDE RETVBTAB                                                       
         EJECT                                                                  
*              VALIDATE OPTIONS                                                 
         SPACE 3                                                                
VVALOPTS DS    0H                                                               
         MVI   BOXOPT,C'Y'         PRESET VALUES                                
         MVI   LEFTOPT,C'N'                                                     
         MVI   SPACOPT,1                                                        
***      MVI   DOWNOPT,C'N'                                                     
         MVI   DOWNOPT,0                                                        
         MVI   DRINDS,0                                                         
         MVI   FISCOPT,1                                                        
         MVI   THOUOPT,C'N'                                                     
         MVI   WIDEOPT,C'N'                                                     
         MVI   TRACEOPT,C'N'                                                    
         MVI   BRACKOPT,C'N'                                                    
         MVI   GRANDOPT,C'N'                                                    
         MVI   NARROPT,C'N'                                                     
         MVI   TESTOPT,0                                                        
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         MVI   FIELDERR,1                                                       
         GOTO1 SCANNER,DMCB,(R2),(10,BLOCK),0                                   
         ZIC   R0,4(R1)                                                         
         LA    R4,BLOCK                                                         
         LTR   R0,R0                                                            
         BZ    BADOPT                                                           
         SPACE                                                                  
OPT00    CLC   12(3,R4),=C'BOX'    BOX OPTION                                   
         BNE   OPT04                                                            
         MVC   BOXOPT,22(R4)                                                    
         B     OPTEND                                                           
         SPACE                                                                  
OPT04    CLC   12(4,R4),=C'LEFT'   LEFT OPTION                                  
         BNE   OPT06                                                            
         MVI   LEFTOPT,C'Y'                                                     
         B     OPTEND                                                           
         SPACE                                                                  
OPT06    CLC   12(2,R4),=C'S   '   SPACING OPTION                               
         BNE   OPT08                                                            
         MVC   SPACOPT,11(R4)                                                   
         CLI   SPACOPT,1                                                        
         BL    BADOPT                                                           
         CLI   SPACOPT,3                                                        
         BH    BADOPT                                                           
         B     OPTEND                                                           
         SPACE                                                                  
OPT08    CLC   12(4,R4),=C'DOWN'   DOWNLOADING OPTION                           
         BNE   OPT10                                                            
*** USE DRIVER DOWNLOAD FLAG                                                    
***      MVI   DOWNOPT,C'Y'                                                     
         OI    DOWNOPT,GLDLACTV                                                 
*                                                                               
         CLC   16(4,R4),=C'HEAD'                                                
         BNE   *+8                                                              
         OI    DOWNOPT,GLDLHEAD                                                 
*                                                                               
         CLC   16(3,R4),=C'AUR'                                                 
         BNE   *+8                                                              
         OI    OPTIND1,AURDOWN                                                  
*                                                                               
         OI    REQRTYP,REQTDOWN    SET FOR PQ IDENT                             
         SPACE                                                                  
         CLI   CONOUT,C' '         SEE IF ANY OUTPUT TYPE REQUESTED             
         BH    OPTEND                                                           
         MVC   CONOUT(8),=CL8'DOWN' FORCE OUTPUT TO 'DOWN'                      
         OI    CONOUTH+6,X'80'                                                  
         MVI   CONOUTH+5,4                                                      
         MVC   TWAOUT,CONOUT                                                    
         B     OPTEND                                                           
         SPACE                                                                  
OPT10    CLC   12(4,R4),=C'WIDE'   WIDE PRINTING (165)                          
         BNE   OPT16                                                            
         MVI   WIDEOPT,C'Y'                                                     
         B     OPTEND                                                           
         SPACE                                                                  
OPT16    CLC   12(5,R4),=C'TRACE'  TRACE OPTION                                 
         BNE   OPT20                                                            
         MVI   TRACEOPT,C'Y'                                                    
         B     OPTEND                                                           
         SPACE                                                                  
OPT20    CLC   12(5,R4),=C'GRAND'   GRAND TOTAL OPTION                          
         BNE   OPT22                                                            
         MVI   GRANDOPT,C'Y'                                                    
         B     OPTEND                                                           
         SPACE                                                                  
OPT22    CLC   12(6,R4),=C'NARROW'  OPTION                                      
         BNE   OPT24                                                            
         MVI   NARROPT,C'Y'                                                     
         MVI   LEFTOPT,C'Y'         FORCE LEFT OPTION                           
         B     OPTEND                                                           
         SPACE                                                                  
OPT24    CLC   12(6,R4),=C'ALLDET'  OPTION TO GET ALL DETAILS                   
         BNE   OPT28                                                            
         OI    DRINDS,X'02'                                                     
         B     OPTEND                                                           
         SPACE                                                                  
OPT28    CLC   12(6,R4),=C'ALLTOT'  OPTION TO GET ALL TOTALS                    
         BNE   OPT30                                                            
         OI    DRINDS,X'04'                                                     
         B     OPTEND                                                           
         SPACE                                                                  
OPT30    CLC   12(4,R4),=C'TEST'    TEST OPTION                                 
         BNE   OPT40                                                            
         MVC   TESTOPT,22(R4)                                                   
         B     OPTEND                                                           
         SPACE                                                                  
OPT40    CLC   12(8,R4),=C'ACTIVITY'  AUR ACTIVITY OPTION                       
         BNE   OPT50                                                            
         CLC   =C'AUR',CONREC      VALID ONLY FOR AUR                           
         BNE   BADOPT                                                           
         OI    OPTIND1,AURBYACT                                                 
         B     OPTEND                                                           
         SPACE                                                                  
OPT50    CLC   12(4,R4),=C'WEEK'    AUR WEEKLY OPTION                           
         BNE   OPT60                                                            
         CLC   =C'AUR',CONREC      VALID ONLY FOR AUR                           
         BNE   BADOPT                                                           
         OI    OPTIND1,AURWEEK                                                  
         B     OPTEND                                                           
         SPACE                                                                  
OPT60    CLC   12(8,R4),=C'DEMROUND'  AUR ROUNDED DEMO OPTION                   
         BNE   OPT70                                                            
         CLC   =C'AUR',CONREC      VALID ONLY FOR AUR                           
         BNE   BADOPT                                                           
         OI    OPTIND1,DEMROUND                                                 
         B     OPTEND                                                           
         SPACE                                                                  
OPT70    CLC   12(5,R4),=C'ACOST'   AUR STREAMLINED REPORT OPTION               
         BNE   OPT80                                                            
         CLC   =C'AUR',CONREC      VALID ONLY FOR AUR                           
         BNE   BADOPT                                                           
         OI    OPTIND1,AURSHORT                                                 
         B     OPTEND                                                           
         SPACE                                                                  
OPT80    DS    0H                                                               
         B     BADOPT                                                           
         SPACE                                                                  
OPTEND   LA    R4,32(R4)                                                        
         AI    FIELDERR,1                                                       
         BCT   R0,OPT00                                                         
         B     XIT                                                              
         SPACE                                                                  
BADOPT   MVC   CONHEAD(L'OPTERR),OPTERR                                         
         B     MYCURSOR                                                         
         EJECT                                                                  
*              VALIDATE TITLE                                                   
         SPACE 3                                                                
VVALTITS MVC   TITLE,SPACES                                                     
         MVC   TITLE(20),=C'REPPAK REPORT WRITER'                               
         CLI   5(R2),0                                                          
         BE    VVALTIT2                                                         
         GOTO1 ANY                                                              
         MVC   TITLE,WORK                                                       
         SPACE                                                                  
VVALTIT2 CLI   NARROPT,C'Y'                                                     
         BE    XIT                                                              
         GOTO1 CENTER,DMCB,TITLE,64                                             
         SPACE                                                                  
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE LEFT HAND HEADERS                                       
         SPACE 3                                                                
VVALLEFT MVI   ROW1WIDE,0          NOT CHECKING REPORT WIDTH YET                
         MVI   TOTWIDTH,0                                                       
         MVI   MAX,4                                                            
         BAS   RE,DELINS                                                        
         LA    R3,4                MAX 4 FIELDS                                 
         LA    R4,4                (START ON HEAD 4)                            
         SPACE                                                                  
VVL2     CLI   5(R2),0                                                          
         BE    VVL4                                                             
         MVI   MYPOSO,C'H'                                                      
         STC   R4,MYPOSO+1                                                      
         MVI   MYPOSO+2,2          (COLUMN 2)                                   
         BAS   RE,VALROW                                                        
         LA    R4,1(R4)                                                         
         SPACE                                                                  
VVL4     BAS   RE,BUMP                                                          
         BCT   R3,VVL2                                                          
         SPACE                                                                  
         LA    R4,2(R4)                                                         
         CH    R4,=H'8'                                                         
         BH    *+8                                                              
         LA    R4,8                                                             
         STC   R4,MYFIRSTH                                                      
         B     XIT                                                              
         EJECT                                                                  
*              RIGHT SIDE HEADERS & MIDLINES NOT USED *                         
         SPACE                                                                  
*              VALIDATE RIGHT SIDE HEADERS                                      
         SPACE 3                                                                
VVALRGHT LA    R3,3                MAX 3 FIELDS                                 
         LA    R4,6                (START ON HEAD 6)                            
         MVI   MAX,3                                                            
         BAS   RE,DELINS                                                        
         SPACE                                                                  
VVRT2    CLI   5(R2),0                                                          
         BE    VVRT4                                                            
         MVI   MYPOSO,C'H'                                                      
         STC   R4,MYPOSO+1                                                      
         MVI   MYPOSO+2,99         (COLUMN 99)                                  
         CLI   NARROPT,C'Y'        (NOT ALLOWED FOR NARROW)                     
         BE    BADROW                                                           
         CLI   WIDEOPT,C'Y'                                                     
         BNE   *+8                                                              
         MVI   MYPOSO+2,132        (COLUMN 132 FOR WIDE)                        
         BAS   RE,VALROW                                                        
         LA    R4,1(R4)                                                         
         SPACE                                                                  
VVRT4    BAS   RE,BUMP                                                          
         BCT   R3,VVRT2                                                         
         SPACE                                                                  
         LA    R4,2(R4)                                                         
         CH    R4,=H'8'                                                         
         BH    *+8                                                              
         LA    R4,8                                                             
         IC    R3,MYFIRSTH                                                      
         CR    R4,R3                                                            
         BL    XIT                                                              
         STC   R4,MYFIRSTH                                                      
         B     XIT                                                              
         SPACE 3                                                                
*              VALIDATE MID                                                     
         SPACE                                                                  
VVALMID  MVI   MYPOSO,C'M'                                                      
         MVI   MYPOSO+1,1                                                       
         MVI   MYPOSO+2,1                                                       
         BAS   RE,VALROW                                                        
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE ROWS                                                    
         SPACE                                                                  
VVALROWS MVI   TOTWIDTH+1,1        START CHECKING REPORT WIDTH NOW              
         ST    R2,ALASTCOL         (REALLY A(FIRST ROW!)                        
         ZIC   R3,MAX                                                           
         LTR   R3,R3                                                            
         BNZ   *+8                                                              
         LA    R3,8                                                             
         STC   R3,MAX                                                           
         BAS   RE,DELINS           CHECK FOR DELETES/INSERT                     
         SPACE                                                                  
VVR2     XC    MYPOSO,MYPOSO                                                    
         BAS   RE,VALROW                                                        
         ZIC   R0,TOTWIDTH+1                                                    
         BCTR  R0,0                                                             
         CLI   ROW1WIDE,0                                                       
         BNE   *+8                                                              
         STC   R0,ROW1WIDE                                                      
         BAS   RE,BUMP                                                          
         BCT   R3,VVR2                                                          
         STC   R0,ROWWIDTH                                                      
         L     R2,ALASTCOL                                                      
         CLI   ANYROWSW,C'N'                                                    
         BE    BADNEED1                                                         
         B     XIT                                                              
         EJECT                                                                  
*              ROWS - FIRST VALIDATE FOR KEYWORD                                
         SPACE 3                                                                
VALROW   NTR1                                                                   
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         MVI   ANYROWSW,C'Y'                                                    
         GOTO1 SCANNER,DMCB,(R2),(6,BLOCK),0                                    
         ZIC   R0,4(R1)                                                         
         LTR   R0,R0                                                            
         BZ    BADROW                                                           
         MVI   FIELDERR,1                                                       
         LA    R4,BLOCK                                                         
         SPACE                                                                  
         GOTO1 VROWDRON            VALIDATE A ROW ENTRY                         
         CLI   DRERROR,0                                                        
         BNE   BADROW                                                           
         CLI   DRATTRIB,C'C'       COLUMN ONLY ENTRIES NOT ALLOWED              
         BE    BADROW2                                                          
         SPACE                                                                  
*        CLC   12(4,R4),=C'RANK'                                                
*        BNE   VROW2                                                            
*        LA    R4,32(R4)           RANK NEEDS A COMPUTE EXPRESSION              
*        BCT   R0,*+8                                                           
*        B     BADROW                                                           
*        AI    FIELDERR,1                                                       
*        MVI   DRCMPMAX,C'P'                                                    
*        CLI   OFFLINE,C'Y'                                                     
*        BNE   VROW1                                                            
*        GOTO1 GROWDRON                                                         
*        GOTO1 GCMPDRON                                                         
*        CLI   DRERROR,0                                                        
*        BNE   BADROW                                                           
*        B     XIT                                                              
         SPACE                                                                  
*ROW1    GOTO1 VCMPDRON            VALIDATE A COMPUTE EXPRESSION                
*        CLI   DRERROR,0                                                        
*        BNE   BADROW                                                           
*        OC    TOTWIDTH,TOTWIDTH   IF WE ARE IN THE ROWS                        
*        BZ    VROW2                                                            
*        CH    R3,=H'1'            CHECK THIS IS NOT THE LAST ROW               
*        BE    BADLRANK                                                         
*        LR    R3,R2                                                            
*        BAS   RE,BUMP                                                          
*        CLI   5(R2),0             AND THERE IS INPUT IN NEXT                   
*        LR    R2,R3                                                            
*        BE    BADLRANK            NOT GOOD TO RANK ON LAST ROW                 
         SPACE                                                                  
VROW2    CLI   MYPOSO,C'H'         SPECIAL FOR HEADS                            
         BNE   VROW4                                                            
         OI    DRFIRST,X'80'       GENERATE FIRST STATEMENT                     
         OI    DRFOPTS,DRFSKIP     WITH SKIP OPTION                             
         MVI   DRFSPACE,0                                                       
         B     VROWNXT                                                          
         SPACE                                                                  
VROW4    CLI   MYPOSO,C'M'         SPECIAL FOR MID                              
         BNE   VROWNXT                                                          
*******  OI    DRFIRST,X'80'       GENERATE FIRST STATEMENT                     
*******  MVI   DRLSPACE,1          WITH ONE SPACE                               
         B     VROWNXT                                                          
         EJECT                                                                  
*              CHECK FOR SUBSIDIARY ROW EXPRESSIONS                             
         SPACE 3                                                                
VROW12   CLC   12(2,R4),=C'* '     TOTAL EXPRESSION                             
         BNE   VROW14                                                           
         OI    DRTOTAL,X'80'                                                    
*******  MVI   DRTSPACE,1          SPACE AFTER TOTALS                           
         LR    RE,R2                                                            
         SPACE                                                                  
VROW12B  ZIC   RF,0(RE)                                                         
         AR    RE,RF               HAVE A LOOK AT THE NEXT LEVEL                
         CLI   0(RE),0                                                          
         BE    VROWNXT                                                          
         TM    1(RE),X'20'                                                      
         BO    VROW12B             FIND AN UNPROTECTED FIELD                    
         ZIC   RF,5(RE)                                                         
         LTR   RF,RF               WITH SOME DATA                               
         BZ    VROW12B                                                          
         LA    RE,8(RE)                                                         
         SPACE                                                                  
VROW12D  CLI   0(RE),C'*'          IF A TOTAL IS NOT SPECIFIED                  
         BE    VROWNXT                                                          
         LA    RE,1(RE)                                                         
         BCT   RF,VROW12D                                                       
*****    OI    DRLAST,X'80'        GENERATE A SPACE BEFORE TOTALS               
*****    MVI   DRLSPACE,1                                                       
         B     VROWNXT                                                          
         SPACE                                                                  
VROW14   CLC   12(5,R4),=C'SKIP '  SKIP TO CHANNEL 1 AFTER BREAK                
         BNE   VROW16                                                           
         OI    DRFIRST,X'80'       GENERATE FIRST STATEMENT                     
         OI    DRFOPTS,DRFSKIP     WITH SKIP OPTION                             
         B     VROWNXT                                                          
         SPACE                                                                  
VROW16   CLC   12(6,R4),=C'SPACE ' SPACE OPTION                                 
         BNE   VROW18                                                           
         OI    DRLAST,X'80'        GENERATE LAST STATEMENT                      
         MVI   DRLSPACE,1          WITH AT LEAST ONE SPACE                      
         CLI   1(R4),0             CHECK SECOND PARAMETER                       
         BE    VROWNXT                                                          
         MVC   DRLSPACE,11(R4)                                                  
         CLI   DRLSPACE,0          S/B 1-3 LINES                                
         BE    BADROW                                                           
         CLI   DRLSPACE,3                                                       
         BH    BADROW                                                           
         B     VROWNXT                                                          
         SPACE                                                                  
VROW18   CLC   12(2,R4),=C'U '                                                  
         BNE   VROW20                                                           
         BAS   RE,VUSRDRON                                                      
         B     VROWNXT                                                          
         SPACE                                                                  
VROW20   CLC   12(3,R4),=C'NP '    OPTION NOT TO PRINT                          
         BNE   VROW22                                                           
         NI    DRFLAGO,X'7F'                                                    
         NI    DRHEAD1,X'7F'                                                    
         NI    DRHEAD2,X'7F'                                                    
         NI    DRHEAD3,X'7F'                                                    
         NI    DRHEAD4,X'7F'                                                    
         B     VROWNXT                                                          
         SPACE                                                                  
VROW22   CLC   12(4,R4),=C'DET '   TOTAL DETAIL EXPRESSION                      
         BE    VROW23                                                           
         CLC   12(2,R4),=C'D '     DET=N OR D=N FORMAT                          
         BNE   VROW24                                                           
         SPACE                                                                  
VROW23   OI    DRTOTAL,X'80'                                                    
         MVI   DRTSPACE,1          SPACE AFTER TOTALS                           
         MVC   DRTDET(1),11(R4)    PICK UP NUMBER OF DETAILS                    
         CLI   DRTDET,0            MUST BE SOMETHING NUMERIC                    
         BE    BADROW                                                           
         B     VROWNXT                                                          
         SPACE                                                                  
VROW24   TM    2(R4),X'80'         NUMERIC=OUTPUT WIDTH OVERRIDE                
         BNO   VROW26                                                           
         L     R1,4(R4)            OUTPUT WIDTH OVERRIDE                        
         STC   R1,DRLENO           (NEW OUTPUT LENGTH)                          
         B     VROWNXT                                                          
         SPACE                                                                  
VROW26   B     BADROW                                                           
         SPACE                                                                  
VROWNXT  LA    R4,32(R4)                                                        
         AI    FIELDERR,1                                                       
         BCT   R0,VROW12                                                        
         EJECT                                                                  
*              FINAL ADJUSTMENTS                                                
         SPACE 3                                                                
         OC    TOTWIDTH,TOTWIDTH   IF WE ARE CHECKING WIDTH                     
         BZ    VROWGEN                                                          
         TM    DRFLAGO,X'80'                                                    
         BNO   VROWGEN                                                          
         LH    R1,TOTWIDTH         ADJUST CURRENT WIDTH                         
         ZIC   RF,DRLENO                                                        
         AR    R1,RF                                                            
         LA    R1,1(R1)                                                         
         STH   R1,TOTWIDTH                                                      
         SPACE                                                                  
VROWGEN  CLI   OFFLINE,C'Y'        NOW GENERATE ELEMENTS                        
         BNE   XIT                                                              
         MVC   DRPOSO,MYPOSO                                                    
         SPACE                                                                  
         TM    DRTOTAL,X'80'       WAS TOTAL REQUESTED?                         
         BNO   VROWADJ2                                                         
         CLI   DRRTNO,X'41'        IF OUT ROUTINE SPECIFIED                     
         BL    VROWADJ2                                                         
*******  CLI   DRLENO,14              AND LENGTH IS AT LEAST 14                 
*******  BL    VROWADJ2                                                         
         MVC   DRTRTN,DRRTNO       USE THIS FOR TOTAL AS WELL                   
         MVC   DRTARGS,DRARGSO     AND PASS THROUGH THE ARGUMENTS               
         MVI   DRTNARGS,16                                                      
         MVI   DRTLITLN,0                                                       
         SPACE                                                                  
VROWADJ2 GOTO1 GROWDRON                                                         
         B     XIT                                                              
         SPACE                                                                  
BADROW   MVC   CONHEAD(L'ROWERR),ROWERR                                         
         B     MYCURSOR                                                         
         SPACE                                                                  
BADROW2  MVC   CONHEAD(L'COLONLY),COLONLY                                       
         B     MYCURSOR                                                         
         SPACE                                                                  
         EJECT                                                                  
*              VALIDATE COLUMNS                                                 
         SPACE 3                                                                
VVALCOLS ZIC   R0,MAX                                                           
         LTR   R0,R0                                                            
         BZ    XIT                                                              
         CLI   5(R2),0                                                          
         BE    BADNEED1            NEED AT LEAST 1 COLUMN                       
         BAS   RE,DELINS           CHECK FOR DELETES/INSERT                     
         MVI   MYLABEL,C'A'                                                     
         ST    R2,ALASTCOL                                                      
         BAS   RE,SETMAX           SET LAST COLUMN FOR COMPUTE                  
         LA    R3,EDITLIST                                                      
         MVI   MYCOLNUM,1                                                       
         MVI   FILTSLOT,1                                                       
         SPACE                                                                  
VVALCOL2 XC    MYPOSO,MYPOSO                                                    
         MVC   0(1,R3),MYLABEL     SAVE LABEL IN EDIT LIST                      
         BAS   RE,VALCOL                                                        
         BAS   RE,BUMP                                                          
         CLI   CLEXTEND,2          UNLESS THIS IS A CONTINUATION                
         BE    *+10                                                             
         MVC   MYLABEL,8(R2)       SAVE THE LABEL                               
         BAS   RE,BUMP                                                          
         LA    R3,4(R3)                                                         
         AI    MYCOLNUM,1                                                       
         BCT   R0,VVALCOL2                                                      
         SPACE                                                                  
         CLC   TOTWIDTH,=H'80'     CHECK NOT TOO BIG NOW                        
         BNH   XIT                                                              
         CLI   NARROPT,C'Y'        ONLY 80 ALLOWED WITH NARROW OPT              
         BE    VVALCBIG                                                         
         CLC   TOTWIDTH,=H'132'    CHECK NOT TOO BIG NOW                        
         BNH   XIT                                                              
         CLI   WIDEOPT,C'Y'                                                     
         BNE   VVALCBIG                                                         
         CLC   TOTWIDTH,=H'165'                                                 
         BNH   XIT                                                              
         SPACE                                                                  
VVALCBIG MVC   CONHEAD(36),=C'REPORT HAS NNN CHARACTERS - TOO WIDE'             
         LA    R3,CONHEAD+11                                                    
         EDIT  (2,TOTWIDTH),(3,(R3))                                            
         L     R2,ALASTCOL                                                      
         B     MYEND                                                            
         EJECT                                                                  
*              VALIDATE FIRST COLUMN EXPRESSION                                 
         SPACE 3                                                                
VALCOL   NTR1                                                                   
         MVI   FIELDERR,1                                                       
         CLI   CLEXTEND,2          HANDLING EXTENSION HERE                      
         BNE   VCOLB                                                            
         CLI   5(R2),0                                                          
         BE    VCOLNXT2                                                         
         LA    R4,BLOCK+42+42      (DON'T DISTURB FIRST 2 ENTRIES               
         B     VCOL1               FOR EXTEND - COMPUTES MAY BE THERE)          
         SPACE                                                                  
VCOLB    CLI   5(R2),0                                                          
         BE    XIT                                                              
         ST    R2,ALASTCOL                                                      
         XC    DRARGSI,DRARGSI                                                  
         XC    BLOCK(252),BLOCK                                                 
         LA    R4,BLOCK+42                                                      
         MVI   CLEXTEND,0                                                       
         ZIC   R1,5(R2)                                                         
         LA    R1,8-1(R1,R2)       (R1=A(LAST CHARACTER))                       
         CLI   0(R1),C','          IF THIS IS A COMMA                           
         BNE   VCOL1                                                            
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         STC   R1,5(R2)            REDUCE APPARENT LENGTH                       
         MVI   CLEXTEND,1          AND NOTE THAT THERE IS AN EXTENSION          
         SPACE                                                                  
VCOL1    GOTO1 SCANNER,DMCB,(20,(R2)),(5,(R4)),0                                
         ZIC   R0,4(R1)                                                         
         LTR   R0,R0                                                            
         BZ    BADCOL                                                           
         SPACE                                                                  
         CLI   CLEXTEND,2                                                       
         BE    VCOL12                                                           
         GOTO1 VCOLDRON            VALIDATE A COLUMN ENTRY                      
         CLI   DRERROR,0                                                        
         BNE   VCOL4                                                            
         CLI   DRATTRIB,C'R'       ROW ONLY ENTRIES NOT ALLOWED                 
         BE    BADCOL2                                                          
         MVI   DRNARGSI,16                                                      
         CLI   1(R4),0             ENTRY=XXXX IS BAD                            
         BE    VCOLNXT                                                          
         B     BADCOL                                                           
         SPACE                                                                  
VCOL4    XC    BLOCK(42),BLOCK     MAY BE A COMPUTE EXPRESSION                  
         MVI   BLOCK,7                                                          
         MVC   BLOCK+12(30),SPACES                                              
         MVC   BLOCK+12(7),=C'COMPUTE'                                          
         LA    R4,BLOCK                                                         
         GOTO1 VCOLDRON            VALIDATE THE COMPUTE COLUMN                  
         CLI   DRERROR,0                                                        
         BNE   BADCOL                                                           
         LA    R4,BLOCK+42                                                      
         CLI   OFFLINE,C'Y'                                                     
         BE    VCOL6                                                            
         GOTO1 VCMPDRON            VALIDATE A COMPUTE EXPRESSION                
         CLI   DRERROR,0                                                        
         BNE   BADCOL                                                           
         SPACE                                                                  
VCOL6    BAS   RE,COMPEDIT         AUTO EDIT FOR COMPUTES                       
         B     VCOLNXT                                                          
         SPACE                                                                  
SETMAX   NTR1                                                                   
         MVI   BYTE,C'A'           FIND LAST INPUT COLUMN                       
         SPACE                                                                  
SETMAX2  CLI   5(R2),0                                                          
         BE    XIT                                                              
         MVC   DRCMPMAX,BYTE                                                    
         BAS   RE,BUMP                                                          
         MVC   BYTE,8(R2)                                                       
         BAS   RE,BUMP                                                          
         BCT   R0,SETMAX2                                                       
         B     XIT                                                              
         EJECT                                                                  
*              CHECK FOR SUBSIDIARY COLUMN EXPRESSIONS                          
         SPACE 3                                                                
VCOL12   TM    2(R4),X'80'         NUMERIC=OUTPUT WIDTH OVERRIDE                
         BNO   VCOL14                                                           
         CLI   7(R4),61            61,62 ARE PERIOD EXPRESSIONS                 
         BE    VCOL14                                                           
         CLI   7(R4),62                                                         
         BE    VCOL14                                                           
         L     R1,4(R4)            OUTPUT WIDTH OVERRIDE                        
         STC   R1,DRLENO           (NEW OUTPUT LENGTH)                          
         B     VCOLNXT                                                          
         SPACE                                                                  
*                                  CHECK FOR PERIOD EXPRESSION                  
VCOL14   BAS   RE,PERVAL1                                                       
         BE    VCOLNXT             (PERVAL FOUND SOMETHING GOOD)                
         SPACE                                                                  
VCOL18   LA    R1,DRHEAD1          CHECK FOR HEADING OVERRIDES                  
         CLC   12(2,R4),=C'H '                                                  
         BE    VCOL20                                                           
         CLC   12(3,R4),=C'H1 '                                                 
         BE    VCOL20                                                           
         LA    R1,DRHEAD2                                                       
         CLC   12(3,R4),=C'H2 '                                                 
         BE    VCOL20                                                           
         LA    R1,DRHEAD3                                                       
         CLC   12(3,R4),=C'H3 '                                                 
         BE    VCOL20                                                           
         LA    R1,DRHEAD4                                                       
         CLC   12(3,R4),=C'H4 '                                                 
         BNE   VCOL24                                                           
         SPACE                                                                  
VCOL20   XC    0(64,R1),0(R1)      TURN OFF ROUTINE & ARGS                      
         CLI   1(R4),0                                                          
         BE    VCOLNXT             HN= CAUSES REMOVAL                           
         OI    0(R1),X'80'         OTHERWISE TURN IT BACK ON                    
         MVC   27(1,R1),1(R4)      PASS LITERAL LENGTH TO DRONE                 
         CLC   1(1,R4),DRLENO                                                   
         BNH   VCOL22              CHECK LITERAL NOT WIDER THAN COLUMN          
         MVC   CONHEAD(L'HOVERR),HOVERR                                         
         B     MYCURSOR                                                         
         SPACE                                                                  
VCOL22   MVC   28(24,R1),22(R4)    PASS DRONE THE LITERAL                       
         B     VCOLNXT                                                          
         SPACE                                                                  
VCOL24   CLC   12(3,R4),=C'NP '    OPTION NOT TO PRINT                          
         BNE   VCOL30                                                           
         MVI   MYPOSO,C'N'                                                      
         NI    DRHEAD1,X'7F'                                                    
         NI    DRHEAD2,X'7F'                                                    
         NI    DRHEAD3,X'7F'                                                    
         NI    DRHEAD4,X'7F'                                                    
         B     VCOLNXT                                                          
         SPACE                                                                  
VCOL30   CLC   12(2,R4),=C'U '     USER RECORD                                  
         BNE   VCOL34                                                           
         BAS   RE,VUSRDRON                                                      
         B     VCOLNXT                                                          
         EJECT                                                                  
*              SUPPORT COLUMN FILTERS                                           
         SPACE 3                                                                
VCOL34   ZIC   R1,0(R4)            L'FILTER EXPRESSION                          
         BCTR  R1,0                                                             
         LA    RE,CFILTTAB                                                      
         SPACE                                                                  
VCOL36   EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),12(R4)                                                   
         BE    VCOL38                                                           
         LA    RE,L'CFILTENT(RE)                                                
         CLI   0(RE),X'FF'                                                      
         BE    VCOL44                                                           
         B     VCOL36                                                           
         SPACE                                                                  
VCOL38   LA    R1,DRARGSI+6        UP TO 3 SLOTS SUPPORTED                      
         LA    RF,3                                                             
         SPACE                                                                  
VCOL40   CLI   0(R1),0                                                          
         BE    VCOL42                                                           
         LA    R1,1(R1)                                                         
         BCT   RF,VCOL40                                                        
         B     BADCOL                                                           
         SPACE                                                                  
VCOL42   MVC   0(1,R1),FILTSLOT    SAVE FILTER SLOT NUMBER                      
         ZIC   R1,FILTSLOT                                                      
         LA    R1,1(R1)                                                         
         STC   R1,FILTSLOT         USE SLOT NUMBER                              
         SH    R1,=H'2'                                                         
         SLL   R1,4                                                             
         A     R1,ACOLFILT         TO DISPLACE INTO COLUMN FILTERS              
         MVC   0(2,R1),8(RE)       SAVE DISPLACEMENT AND LENGTH                 
         MVC   4(12,R1),22(R4)     AND SAVE FILTERED DATA                       
         CLI   22(R4),C'-'                                                      
         BNE   VCOLNXT                                                          
         MVC   4(12,R1),23(R4)     (NEGATIVE DATA)                              
         MVI   3(R1),C'-'                                                       
         B     VCOLNXT                                                          
         SPACE                                                                  
VCOL44   B     BADCOL                                                           
         EJECT                                                                  
*              ALLOWABLE COLUMN FILTER EXPRESSIONS                              
         SPACE 3                                                                
*                EXPRESSION    DISPLACEMENT       LENGTH                        
         SPACE                                                                  
CFILTTAB DS    0H                                                               
CFILTENT DS    0CL10                                                            
         DC    C'OFFICE  ',AL1(REOFF-RECODES),AL1(L'REOFF)                      
         DC    C'SALES   ',AL1(RESAL-RECODES),AL1(L'RESAL)                      
         DC    C'AGENCY  ',AL1(REAGY-RECODES),AL1(L'REAGY)                      
         DC    C'ADVERTSR',AL1(READV-RECODES),AL1(L'READV)                      
         DC    C'STATION ',AL1(RESTA-RECODES),AL1(L'RESTA)                      
         DC    C'CONTYPE ',AL1(RECTY-RECODES),AL1(L'RECTY)                      
         DC    C'REGION  ',AL1(REREG-RECODES),AL1(L'REREG)                      
         DC    C'DIVTEAM ',AL1(REDVT-RECODES),AL1(L'REDVT)                      
         DC    C'GRPSUB  ',AL1(REGRS-RECODES),AL1(L'REGRS)                      
         DC    X'FF'                                                            
         EJECT                                                                  
*              END OF COLUMN VALIDATION                                         
         SPACE 3                                                                
VCOLNXT  LA    R4,42(R4)                                                        
         AI    FIELDERR,1                                                       
         BCT   R0,VCOL12                                                        
         SPACE                                                                  
         CLI   CLEXTEND,1          IF THERE IS AN EXTENSION PENDING             
         BNE   VCOLNXT2                                                         
         MVI   CLEXTEND,2             NOT TIME TO WRAP UP YET                   
         B     XIT                                                              
         SPACE                                                                  
VCOLNXT2 MVI   CLEXTEND,0                                                       
         SPACE                                                                  
         CLI   MYPOSO,C'N'         IF THERE IS ANY PRINTING                     
         BE    VCOLGEN                                                          
         LH    R1,TOTWIDTH         ADJUST CURRENT WIDTH                         
         ZIC   RF,DRLENO                                                        
         AR    R1,RF                                                            
         LA    R1,1(R1)                                                         
         STH   R1,TOTWIDTH                                                      
         SPACE                                                                  
VCOLGEN  CLI   OFFLINE,C'Y'        NOW GENERATE ELEMENTS                        
         BNE   XIT                                                              
         MVC   DRLABELI,MYLABEL                                                 
         MVC   1(1,R3),DRDECO      SAVE EDIT CHARACTERISTICS                    
         MVC   2(1,R3),DRDIVO                                                   
         MVC   DRPOSO,MYPOSO                                                    
         GOTO1 GCOLDRON                                                         
         CLC   BLOCK+12(7),=C'COMPUTE'                                          
         BNE   XIT                                                              
         LA    R4,BLOCK+42                                                      
         GOTO1 GCMPDRON                                                         
         CLI   DRERROR,0                                                        
         BNE   BADCOL                                                           
         B     XIT                                                              
         SPACE                                                                  
BADCOL   MVC   CONHEAD(L'COLERR),COLERR                                         
         B     MYCURSOR                                                         
         SPACE                                                                  
BADUSER  MVC   CONHEAD(L'USRERR),USRERR                                         
         B     MYCURSOR                                                         
         SPACE                                                                  
BADCOL2  MVC   CONHEAD(L'ROWONLY),ROWONLY                                       
         B     MYCURSOR                                                         
         SPACE                                                                  
BADLRANK MVC   CONHEAD(L'LRANKERR),LRANKERR                                     
         B     MYEND                                                            
         SPACE                                                                  
BADNEED1 MVC   CONHEAD(L'NEED1ERR),NEED1ERR                                     
         B     MYEND                                                            
         EJECT                                                                  
*              ROUTINE TO EDIT COLUMNAR PERIOD EXPRESSION                       
         SPACE 3                                                                
*              INPUT               R4=A(SCANNER LINE)                           
*              OUTPUT              DRARGSI                                      
*                                  10=TYPE  PAY=0 LAST=1 BILL=2                 
*                                           CHECK=3 DUE=4                       
*                                  11-13    START YMD (PWOS)                    
*                                  14-16    END YMD (PWOS)                      
         SPACE 1                                                                
PERVAL1  NTR1                                                                   
         LA    R2,22(R4)                                                        
         ZIC   R3,1(R4)                                                         
         SR    R1,R1                                                            
         LA    R2,12(R4)                                                        
         SR    R1,R1                                                            
         ZIC   R3,0(R4)                                                         
         CLI   1(R4),0                                                          
         BNE   NOGOOD                                                           
         SPACE 1                                                                
PERVAL2  STC   R1,DRARGSI+9        SAVE TYPE                                    
         LTR   R3,R3               R2=A(PERIOD EXPRESS) R3=LENGTH               
         BZ    NOGOOD                                                           
*                                  START DATE                                   
         LA    R1,DRARGSI+11       MUST START WITH A MONTH                      
         BAS   RE,PERMON                                                        
         BNE   NOGOOD                                                           
         BAS   RE,PERYEAR                                                       
         BNE   NOGOOD                                                           
         SH    R3,=H'3'                                                         
         BZ    ITSFINE                                                          
         LA    R2,3(R2)                                                         
         BAS   RE,PERDAY           OPTIONAL DAY                                 
         BNE   NOGOOD                                                           
         ZIC   R1,WORK             (LENGTH RETURNED IN WORK)                    
         AR    R2,R1                                                            
         SR    R3,R1                                                            
         BZ    ITSFINE                                                          
         CLI   0(R2),C'-'                                                       
         BNE   NOGOOD              DATA LENGTH - MUST BE A DASH                 
         LA    R2,1(R2)                                                         
         BCT   R3,*+8                                                           
         B     NOGOOD                                                           
*                                  END DATE NOW                                 
         LA    R1,DRARGSI+14       MUST START WITH A MONTH                      
         BAS   RE,PERMON                                                        
         BNE   NOGOOD                                                           
         BAS   RE,PERYEAR                                                       
         BNE   NOGOOD                                                           
         SH    R3,=H'3'                                                         
         BZ    ITSFINE                                                          
         LA    R2,3(R2)                                                         
         BAS   RE,PERDAY           OPTIONAL DAY                                 
         BNE   NOGOOD                                                           
         ZIC   R1,WORK             (LENGTH RETURNED IN WORK)                    
         SR    R3,R1                                                            
         BNZ   NOGOOD                                                           
         CLC   DRARGSI+10(3),DRARGSI+13                                         
         BH    NOGOOD                                                           
         B     ITSFINE                                                          
         EJECT                                                                  
*              SUBROUTINES FOR PERVAL                                           
         SPACE 1                                                                
PERYEAR  NTR1                                                                   
         BCTR  R1,0                R1=A(YMD) PWOS IN ARGS                       
         OC    REQPEND,REQPEND     IF NO END DATE SPECIFIED                     
         BNZ   PERYEAR2                                                         
         CLC   1(1,R1),REQPSTR+1   MONTH MUST MATCH                             
         BNE   NOGOOD                                                           
         MVC   0(1,R1),REQPSTR     PASS BACK START YEAR                         
         B     ITSFINE                                                          
         SPACE 1                                                                
PERYEAR2 CLC   REQPSTR(1),REQPEND  IF REQUEST S/E SAME YEAR                     
         BNE   PERYEAR4                                                         
         CLC   1(1,R1),REQPSTR+1      MONTH MUST BE WITHIN PERIOD               
         BL    NOGOOD                                                           
         CLC   1(1,R1),REQPEND+1                                                
         BH    NOGOOD                                                           
         MVC   0(1,R1),REQPSTR     PASS BACK START YEAR                         
         B     ITSFINE                                                          
         SPACE 1                                                                
*                                  REQUEST END IN FOLLOWING YEAR                
PERYEAR4 MVC   0(1,R1),REQPSTR     PASS BACK START YEAR                         
         CLC   1(1,R1),REQPSTR+1        IF MONTH AFTER REQUEST START            
         BNL   ITSFINE                                                          
         MVC   0(1,R1),REQPEND     PASS BACK END YEAR                           
         CLC   1(1,R1),REQPEND+1        IF MONTH BEFORE REQUEST END             
         BNH   ITSFINE                                                          
         B     NOGOOD              OTHERWISE ITS NOT OK                         
         SPACE 1                                                                
PERMON   NTR1                                                                   
         LA    R3,MONTHS                                                        
         LA    R4,1                                                             
         LA    R0,12                                                            
         SPACE 1                                                                
PERMON2  STC   R4,0(R1)            PASS BACK MONTH NUMBER                       
         CLC   0(3,R2),0(R3)                                                    
         BE    ITSFINE                                                          
         LA    R3,3(R3)                                                         
         LA    R4,1(R4)                                                         
         CH    R4,=H'10'                                                        
         BNE   *+8                                                              
         LA    R4,X'10'            (RETURNING MONTH IN PWOS)                    
         BCT   R0,PERMON2                                                       
         B     NOGOOD                                                           
         SPACE 1                                                                
PERDAY   NTR1                                                                   
*                                   R2=A(POSSIBLE DAY)                          
*                                   R1=A(MONTH/DAY IN ARGS)                     
*                                   RETURN DAY AND LENGTH IN WORK               
         MVI   WORK,0                                                           
         CLI   0(R2),C'-'                                                       
         BE    ITSFINE                                                          
         CLI   0(R2),X'F0'                                                      
         BL    NOGOOD                                                           
         MVI   WORK,1                                                           
         CLI   1(R2),C' '                                                       
         BE    PERDAY2                                                          
         CLI   1(R2),C'-'                                                       
         BE    PERDAY2                                                          
         CLI   1(R2),X'F0'                                                      
         BL    NOGOOD                                                           
         MVI   WORK,2                                                           
         SPACE 1                                                                
PERDAY2  ZIC   RF,WORK                                                          
         EX    RF,*+8                                                           
         PACK  DUB(2),0(R2)        CONVERT TO PWOS                              
         MVC   1(1,R1),DUB                                                      
         ZIC   RF,0(R1)            PICK UP MONTH (IN PWOS)                      
         LA    RF,DAYMONS-1(RF)    DISPLACE TO DAYS IN MONTH                    
         CLI   1(R1),0             DAYS CAN'T BE ZERO                           
         BE    NOGOOD                                                           
         CLC   1(1,R1),0(RF)       OR MORE THAN DAYS IN MONTH                   
         BH    NOGOOD                                                           
         B     ITSFINE                                                          
         SPACE 1                                                                
DAYMONS  DC    X'312931303130'     MONTHS X'01' TO X'06'                        
         DC    X'313130'                  X'07' TO X'09'                        
         DC    6X'00'                                                           
         DC    X'313031'                  X'10' TO X'12'                        
         EJECT                                                                  
*              ROUTINE TO FIGURE OUT EDITS FOR COMPUTES                         
         SPACE 3                                                                
COMPEDIT NTR1                                                                   
         CLI   BRACKOPT,C'Y'       IF USER ASKED FOR BRACKETS                   
         BNE   *+12                                                             
         MVI   DRFLOATO,0          DO NOT FLOAT IN MINUS AS WELL!               
         OI    DROPTSO,DRBKMINO    AND ENSURE BRACKET OPTION                    
*                                  R4=A(SCANNER TABLE ENTRY)                    
         ZIC   R1,0(R4)            PICK UP EXPRESSION LENGTH                    
         LA    R1,10(R1,R4)                                                     
         CLI   0(R1),C'%'          IS LAST OPERATOR PERCENT?                    
         BE    COMPPCT                                                          
         CLI   0(R1),C'I'          OR INDEX?                                    
         BE    COMPINX                                                          
         BCTR  R1,0                                                             
         CLI   0(R1),C'V'          OR VERTICAL PERCENT                          
         BE    COMPPCT                                                          
         LA    RE,8(R2)            RE=A(START OF INPUT STRING)                  
         ZIC   R0,5(R2)            R0=LENGTH OF INPUT STRING                    
         SPACE                                                                  
COMPED1  LA    R1,EDITLIST         ELSE LOOK FOR FIRST OPERAND                  
COMPED2  CLI   0(R1),0                                                          
         BNE   *+14                                                             
         LA    RE,1(RE)                                                         
         BCT   R0,COMPED1                                                       
         DC    H'0'                NO VARIABLES IN INPUT STRING                 
         CLC   0(1,R1),0(RE)                                                    
         BE    COMPED3                                                          
         LA    R1,4(R1)                                                         
         B     COMPED2                                                          
         SPACE                                                                  
COMPED3  MVC   DRDECO,1(R1)        PICK UP ITS EDIT CHARACTERISTIC              
         MVC   DRDIVO,2(R1)                                                     
         B     XIT                                                              
         SPACE                                                                  
COMPPCT  MVI   DRDECO,2            PERCENTS HAVE 2 DEC                          
         MVI   DRTRAILO,C'%'       AND END WITH PERCENT SIGN                    
         B     XIT                                                              
         SPACE                                                                  
COMPINX  MVI   DRDECO,0            INDEXES HAVE 0 DEC                           
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO ADJUST PERIOD                                         
         SPACE 3                                                                
PERADJ   NTR1                                                                   
         CLC   WORK(6),=C'000000' IF START NOT SPECIFIED                        
         BNE   PERADJ2                                                          
         MVC   WORK(2),WORK+6      COPY YEAR FROM END                           
         MVC   WORK+2(2),=C'01'    USE JANUARY                                  
         MVC   WORK+4(2),=C'01'    MAKE DAY 01                                  
         B     XIT                                                              
         SPACE                                                                  
PERADJ2  CLC   WORK+6(6),=C'000000' IF END NOT SPECIFIED                        
         BNE   XIT                                                              
         MVC   WORK+6(2),WORK      COPY YEAR FROM START                         
         MVC   WORK+6+2(2),=C'12'  USE DECEMBER                                 
         MVC   WORK+6+4(2),=C'31'  MAKE DAY 31                                  
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PUT IN PERIOD HEADING SUPPORT                         
         SPACE 3                                                                
PERPOP   NTR1                                                                   
         CLI   MYPOSO,C'N'         NOT NEEDED IF NOT PRINTING                   
         BE    XIT                                                              
         CLI   DRARGSI+11,0        WAS A PERIOD SPECIFIED?                      
         BE    XIT                     (MINIMUM IS 1 MONTH)                     
         LA    R1,DRHEAD1          YES SO FIND AN EMPTY HEADING                 
         TM    0(R1),X'80'                                                      
         BNO   PERPOP2                                                          
         LA    R1,DRHEAD2                                                       
         TM    0(R1),X'80'                                                      
         BNO   PERPOP2                                                          
         LA    R1,DRHEAD3                                                       
         TM    0(R1),X'80'                                                      
         BNO   PERPOP2                                                          
         LA    R1,DRHEAD4                                                       
         TM    0(R1),X'80'                                                      
         BNO   PERPOP2                                                          
         B     XIT                 NO ROOM!                                     
         SPACE                                                                  
PERPOP2  OI    0(R1),X'80'         FOUND A SPACE - SO TURN ON                   
         MVC   1(8,R1),=CL8'PERPOP'                                             
         MVC   9(7,R1),DRARGSI+9   PASS THE PERIOD TYPE, S/E                    
         MVI   25(R1),16           ALL ARGUMENTS                                
         B     XIT                                                              
         SPACE 3                                                                
         EJECT                                                                  
*              GENERAL HEADLINE HOOK ROUTINES                                   
         SPACE 3                                                                
VGENHEAD L     R2,AH1              DEAL WITH MAIN TITLE                         
         LA    R2,32(R2)                                                        
         CLI   WIDEOPT,C'Y'                                                     
         BNE   *+8                                                              
         LA    R2,16(R2)                                                        
         CLI   NARROPT,C'Y'                                                     
         BNE   VGENH10                                                          
         SH    R2,=H'5'                                                         
         L     R1,AH4                                                           
         MVC   BLOCK(24),59(R1)    SAVE H4 RIGHT FOR NARROW                     
         MVC   59(24,R1),SPACES                                                 
         SPACE                                                                  
VGENH10  MVC   0(64,R2),TITLE      (TITLE IS ALREADY CENTERED)                  
         A     R2,PWIDTH                                                        
         GOTO1 UNDERLIN,DMCB,(64,TITLE),(X'BF',(R2))                            
         SPACE                                                                  
         L     R2,AH4              LINE UP THE LEFT SIDE                        
         LA    R2,1(R2)                                                         
         LA    R3,15                                                            
         L     R1,AGLOBAL                                                       
         USING GLOBALD,R1                                                       
         ZIC   R4,GLFHEADL                                                      
         DROP  R1                                                               
         SH    R4,=H'6'                                                         
         BAS   RE,GETLONG                                                       
         LA    R3,16(R2)                                                        
         A     R2,FULL                                                          
         LA    R2,2(R2)                                                         
         BAS   RE,SHUFFLE                                                       
         LA    R3,15                                                            
         BAS   RE,GETLONG                                                       
         LA    R3,16(R2)                                                        
         A     R2,FULL                                                          
         LA    R2,2(R2)                                                         
         BAS   RE,SHUFFLE                                                       
         SPACE                                                                  
         L     R2,AH4              RESTORE HEAD4                                
         LA    R2,96(R2)                                                        
         CLI   WIDEOPT,C'Y'                                                     
         BNE   *+8                                                              
         LA    R2,32(R2)                                                        
         CLI   NARROPT,C'Y'                                                     
         BNE   *+14                                                             
         SH    R2,=H'37'                                                        
         MVC   0(24,R2),BLOCK      (REPLACE SAVED H4 RIGHT)                     
         SPACE                                                                  
         L     R2,AH4              PERIOD TO HEAD4 CENTER                       
         ST    R2,FULL                                                          
         SPACE                                                                  
         LA    R2,66-19(R2)                                                     
         CLI   WIDEOPT,C'Y'                                                     
         BNE   *+8                                                              
         LA    R2,15(,R2)                                                       
         CLI   NARROPT,C'Y'                                                     
         BNE   *+8                                                              
         SH    R2,=H'26'                                                        
         SPACE                                                                  
         OC    APERFLD,APERFLD     WAS PERIOD ENTERED                           
         BZ    VGENH20                                                          
         SPACE                                                                  
         CLI   NARROPT,C'Y'                                                     
         BE    *+10                                                             
         MVC   0(7,R2),=C'FOR THE'                                              
         MVC   8(11,R2),=C'PERIOD FROM'                                         
         MVC   20(6,R2),REQPSTRM                                                
         MVC   27(2,R2),=C'TO'                                                  
         MVC   30(6,R2),REQPENDM                                                
         SPACE                                                                  
         A     R2,PWIDTH                                                        
         L     RF,FULL                                                          
         A     RF,PWIDTH                                                        
         ST    RF,FULL                                                          
         SPACE                                                                  
VGENH20  DS    0H                                                               
         ICM   RE,15,AACTFLD       WERE ACTIVITY DATES ENTERED                  
         BZ    VGENH30                                                          
         LA    R2,5(,R2)                                                        
         CLI   NARROPT,C'Y'                                                     
         BNE   *+8                                                              
         LA    R2,3(,R2)                                                        
         MVC   0(8,R2),=C'ACTIVITY'                                             
         SPACE                                                                  
         GOTO1 DATCON,DMCB,(0,REQAASTR),(5,9(R2))                               
         MVC   18(2,R2),=C'TO'                                                  
         GOTO1 (RF),(R1),(0,REQAAEND),(5,21(R2))                                
         SPACE                                                                  
         L     RF,FULL                                                          
         A     RF,PWIDTH                                                        
         ST    RF,FULL                                                          
         SPACE                                                                  
VGENH30  MVI   BLOCK,C' '                                                       
         MVC   BLOCK+1(197),BLOCK                                               
         LA    R1,BLOCK                                                         
         LR    R0,R1                                                            
         OC    REFILTS,REFILTS     ANY FILTERS                                  
         BZ    XIT                  NO                                          
         SPACE                                                                  
*         BAS   RE,DFTR             GO DISPLAY FILTER(S)                        
         GOTO1 =A(DFTR),RR=RELO                                                 
         SPACE                                                                  
         LA    R2,96               NORMAL WIDTH                                 
         LA    RE,132                                                           
         CLI   WIDEOPT,C'Y'                                                     
         BNE   *+12                                                             
         LA    R2,32(R2)                                                        
         LA    RE,32(RE)                                                        
         CLI   NARROPT,C'Y'                                                     
         BNE   *+12                                                             
         SH    R2,=H'37'                                                        
         SH    RE,=H'37'                                                        
         SPACE                                                                  
         LA    RF,BLOCK(RE)                                                     
         CLI   0(RF),C' '          LOOK FOR END OF FILTERS                      
         BH    *+12                                                             
         BCTR  RF,0                                                             
         BCT   RE,*-10                                                          
         DC    H'0'                                                             
         SPACE                                                                  
         LR    RF,RE               SAVE TOTAL FILTERS LEN                       
         SPACE                                                                  
         CH    RE,=H'36'           HAVE ROOM FOR 36 CHAR                        
         BNH   *+10                                                             
         SH    RE,=H'36'           BACK UP TO LEFT ALL OVER 36                  
         SR    R2,RE                                                            
         SPACE                                                                  
         A     R2,FULL             GET ADDR OF CURRENT HEADER LINE              
         EX    RF,*+8                                                           
         B     XIT                                                              
         MVC   0(0,R2),BLOCK                                                    
         SPACE                                                                  
*        SR    R3,R3               COUNT OF RIGHT BLANKS                        
*        SR    R4,R4                        LEFT                                
*        LA    R2,132              NORMAL WIDTH                                 
*        CLI   WIDEOPT,C'Y'                                                     
*        BNE   *+8                                                              
*        LA    R2,30(R2)                                                        
*        CLI   NARROPT,C'Y'                                                     
*        BNE   *+8                                                              
*        SH    R2,=H'52'                                                        
*        SPACE                                                                  
*        LR    RE,R2                                                            
*        SRL   RE,1                FIND CENTER OF PAGE                          
*        SPACE                                                                  
*        L     R1,FULL             GET ADDR OF CURRENT HEADER LINE              
*        SPACE                                                                  
*        AR    R1,RE               THIS IS CENTER                               
*        LR    R0,RE                                                            
*        LR    RF,R1                                                            
*        CLI   0(R1),C' '          FIND SPACES TO RIGHT OF CENTER               
*        BH    *+14                                                             
*        BCTR  R3,0                                                             
*        LA    R1,1(,R1)                                                        
*        BCT   RE,*-14                                                          
*        SPACE                                                                  
*        LR    R1,RF               SAVE CENTER OF PAGE                          
*        SPACE                                                                  
*        CLI   0(RF),C' '          FIND SPACES TO LEFT OF CENTER                
*        BH    *+12                                                             
*        BCTR  R4,0                                                             
*        BCTR  RF,0                                                             
*        BCT   R0,*-12                                                          
*        SPACE                                                                  
*        LPR   R3,R3               MAKE POSITIVE                                
*        LPR   R4,R4                                                            
*        SPACE                                                                  
*        CR    R3,R4               MORE SPACES TO RIGHT OR LEFT                 
*        BL    *+6                                                              
*        LR    R3,R4                                                            
*        SPACE                                                                  
*        BCTR  R3,0                                                             
*        SLL   R3,1                DOUBLE IT                                    
*        SPACE                                                                  
*        CR    R2,R3               CAN'T BE BIGGER THAN LINE SIZE               
*        BNL   *+6                                                              
*        DC    H'0'                                                             
*        SPACE                                                                  
*        SR    R2,R3               SUBTRACT FROM LINE SIZE                      
*        SRL   R2,1                                                             
*        SPACE                                                                  
*        A     R2,FULL             STORED ADDR OF NEXT HEAD LINE                
         SPACE                                                                  
*        GOTO1 CENTER,DMCB,BLOCK,(R3)                                           
         SPACE                                                                  
*        BCTR  R3,0                                                             
*        EX    R3,*+8                                                           
*        B     XIT                                                              
*        MVC   0(0,R2),BLOCK                                                    
         EJECT                                                                  
*              SUBSIDIARY ROUTINES FOR GENHEAD                                  
         SPACE 3                                                                
GETLONG  NTR1                                                                   
*              INPUTS              R2=A(FIELD ON FIRST LINE)                    
*                                  R3=MAX WIDTH                                 
*                                  R4=NUMBER OF LINES                           
*              OUTPUT              FULL=WIDEST FOUND                            
         SPACE                                                                  
GETLONG2 ST    R3,FULL                                                          
         LTR   R3,R3                                                            
         BZ    XIT                                                              
         LA    R1,0(R2,R3)                                                      
         BCTR  R1,0                R1=END OF PRESENT FIELD                      
         LR    R0,R4                                                            
         SPACE                                                                  
GETLONG4 CLI   0(R1),C' '          SEE IF ANYTHING SIGNIFICANT                  
         BH    XIT                                                              
         A     R1,PWIDTH           ON EACH OF THE LINES                         
         BCT   R0,GETLONG4                                                      
         BCTR  R3,0                                                             
         B     GETLONG2                                                         
         SPACE 3                                                                
SHUFFLE  NTR1                                                                   
*              INPUTS              R2=A(START DATA ON FIRST LINE)               
*                                  R3=A(FROM DATA)                              
*                                  R4=NUMBER OF LINES                           
         SPACE                                                                  
SHUFFLE2 MVC   WORK,0(R3)                                                       
         MVC   0(60,R3),SPACES                                                  
         MVC   0(60,R2),WORK                                                    
         A     R2,PWIDTH                                                        
         A     R3,PWIDTH                                                        
         BCT   R4,SHUFFLE2                                                      
         B     XIT                                                              
         EJECT                                                                  
*              DRONE UTILITIES                                                  
         SPACE 3                                                                
VINTDRON DS    0H                  INITIALIZATION                               
         MVI   DRWHO,DRREPWHO                                                   
         MVI   DRACTION,DRINIT                                                  
         MVC   DRDICT,=CL8'REWRI '                                              
         MVC   DRALTDIC,=CL8'DRIVER'                                            
         MVC   DRCOMFAC,ACOMFACS                                                
         MVC   DRMAXWID,=H'999'    FORCE BIG - I CHECK WIDTH                    
         GOTO1 DRONE,DMCB,DRGEN                                                 
         CLI   GRANDOPT,C'Y'       IF GRAND TOTALS REQUESTED                    
         BNE   XIT                                                              
         MVC   DROLDBUF,DRCURBUF                                                
         L     R1,DRCURBUF                                                      
         MVC   0(2,R1),=X'4802'    DEAL WITH THAT NOW                           
         LA    R1,2(R1)                                                         
         MVC   0(3,R1),=X'871000'                                               
         MVC   3(13,R1),=C'REPORT TOTALS'                                       
         LA    R1,16(R1)                                                        
         ST    R1,DRCURBUF                                                      
         B     XIT                                                              
         SPACE                                                                  
VROWDRON NTR1                      VALIDATE A ROW                               
         MVI   DRACTION,DRROW                                                   
         B     ALLVAL                                                           
         SPACE                                                                  
GROWDRON NTR1                      GENERATE A ROW                               
         MVI   DRACTION,DRGENROW                                                
         B     ALLDRONE                                                         
         SPACE                                                                  
VCOLDRON NTR1                      VALIDATE A COLUMN                            
         MVI   DRACTION,DRCOL                                                   
         B     ALLVAL                                                           
         SPACE                                                                  
GCOLDRON NTR1                      GENERATE A COLUMN                            
         MVI   DRACTION,DRGENCOL                                                
         B     ALLDRONE                                                         
         SPACE                                                                  
VCMPDRON NTR1                      VALIDATE A COMP                              
         MVI   DRACTION,DRCMP                                                   
         B     ALLVAL                                                           
         SPACE                                                                  
GCMPDRON NTR1                      GENERATE A COMP                              
         MVI   DRACTION,DRGENCMP                                                
         B     ALLVAL              (LOOKS LIKE A VALIDATION)                    
         SPACE                                                                  
VWRPDRON DS    0H                  WRAP UP                                      
         MVI   DRACTION,DRWRAPUP                                                
         GOTO1 DRONE,DMCB,DRGEN                                                 
         BAS   RE,TRACDRON         (OPTIONAL TRACE)                             
         B     XIT                                                              
         SPACE                                                                  
VUSRDRON NTR1                      VALIDATE USER RECORD                         
         MVI   DRACTION,DRUSER                                                  
         MVC   DRUSRKEY(2),AGYALPHA         KEY IS AGENCY                       
         MVC   DRUSRKEY+2(8),22(R4)                AND USER CODE                
         GOTO1 DRONE,DMCB,DRGEN                                                 
         CLI   DRERROR,0                                                        
         BNE   BADUSER                                                          
         TM    DROPTSO,DRBKMINO    IF BRACKET=M SPECIFIED                       
         BNO   XIT                                                              
         NI    DROPTSO,X'DF'       DON'T NEED MINUS=YES                         
         CLI   DRFLOATO,C'-'                                                    
         BNE   XIT                                                              
         MVI   DRFLOATO,0                OR FLOAT=-                             
         B     XIT                                                              
         EJECT                                                                  
*              MORE DRONE UTILITIES                                             
         SPACE 3                                                                
ALLVAL   XC    WORK,WORK           GENERATE A PSEUDO TWA HEADER                 
         MVC   WORK+5(1),0(R4)     (PASS THROUGH THE LENGTH)                    
         MVC   WORK+8(30),12(R4)                                                
         LA    R1,WORK                                                          
         ST    R1,DRACCFLD                                                      
         OI    DRFLAGS,DREXPDIC    TELL DRONE TO EXPLODE DICT.                  
         GOTO1 DRONE,DMCB,DRGEN                                                 
         B     XIT                                                              
         SPACE                                                                  
ALLDRONE GOTO1 DRONE,DMCB,DRGEN                                                 
         B     XIT                 USER NEEDS TO TEST DRERROR                   
         SPACE                                                                  
BADDRONE MVC   CONHEAD,DRERRMSG    ERROR - SO SHOW WHAT DRONE PASSED            
         B     MYCURSOR                                                         
         SPACE                                                                  
TRACDRON NTR1                                                                   
         CLI   TRACEOPT,C'Y'       DRONE TRACING OPTION                         
         BNE   XIT                                                              
         CLI   OFFLINE,C'Y'                                                     
         BNE   XIT                                                              
         L     R3,ADPGPROG                                                      
         SPACE                                                                  
TRACD2   CLI   0(R3),0                                                          
         BE    XIT                                                              
         ZIC   R4,1(R3)                                                         
         LTR   R4,R4                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   P(0),0(R3)                                                       
         LA    R4,1(R4)                                                         
         GOTO1 VPRINT,DMCB,P-1,=C'BL01'                                         
         GOTO1 HEXOUT,DMCB,(R3),BLOCK,(R4),=C'SEP'                              
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   P(0),BLOCK                                                       
         GOTO1 VPRINT,DMCB,P-1,=C'BL01'                                         
         LA    R5,BLOCK+1(R4)                                                   
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   P(0),0(R5)                                                       
         BASR  RE,RF                                                            
         MVC   P,SPACES                                                         
         BASR  RE,RF                                                            
         LA    R3,1(R3,R4)                                                      
         B     TRACD2                                                           
         EJECT                                                                  
*              INITIALIZE TO RUN DRIVER                                         
         SPACE                                                                  
*                                  LOADS PHASES                                 
*                                  SETS GLOBAL ADDRESSES                        
         SPACE                                                                  
VINTDRIV CLI   OFFLINE,C'Y'                                                     
         BNE   XIT                                                              
         GOTO1 CALLOV,DMCB,X'9A000000',0,0  LOAD T8219A(GLOBAL STORAGE)         
         L     R4,DMCB                     FOR DRIVER                           
         ST    R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         SPACE                                                                  
         GOTO1 CALLOV,DMCB,0,X'D9000A3A'   LOAD T00A3A (DRIVER)                 
         L     R2,DMCB                                                          
         ST    R2,DRIVER                                                        
         SPACE                                                                  
         GOTO1 CALLOV,DMCB,0,X'D9082111'   LOAD T82111 (WRIIO)                  
         L     RE,DMCB                                                          
         ST    RE,REWRIIO                                                       
         SPACE                                                                  
         GOTO1 CALLOV,DMCB,0,X'D9082113'   LOAD T82113 (REPPAK DRIVER)          
         L     R2,DMCB                                                          
         ST    R2,GLASYSDR                                                      
         ST    RC,GLAWORKD                                                      
         MVC   GLAPROG,ADPGPROG                                                 
         MVI   GLTWORKD,GLTSPOOL                                                
         MVC   GLFHEADL,MYFIRSTH                                                
         MVC   GLSPACE,SPACOPT      PASS THRU SPACING OPT                       
         MVC   GLBOXOPT,BOXOPT                BOX OPTION                        
         MVC   GLLFTOPT,LEFTOPT           AND LEFT OPTION                       
         MVI   GLNORBOX,X'40'       TURN OFF ROW BOXES FOR TOTALS               
***                                                                             
*        CLI   DOWNOPT,C'Y'         OPTION TO DOWNLOAD                          
*        BNE   *+8                                                              
*        MVI   GLDOWNLD,X'80'                                                   
         MVC   GLDOWNLD,DOWNOPT                                                 
***                                                                             
         MVC   GLINDS,DRINDS        PASS THROUGH DRIVER INDICATORS              
         SPACE                                                                  
DRI2     CLI   TRACEOPT,C'Y'        OPTION TO TRACE                             
         BNE   DRI4                                                             
         MVI   GLTRACE,C'Y'                                                     
         SPACE                                                                  
DRI4     DS    0H                                                               
         EJECT                                                                  
*              INITIALIZATION OF PRINT RELATED FIELDS                           
         SPACE 3                                                                
VINTHEAD L     R4,ABOX                                                          
         USING BOXD,R4                                                          
         CLI   WIDEOPT,C'Y'                                                     
         BE    DRIWIDE                                                          
         MVC   BOXWIDTH,=F'132'                                                 
         MVI   BOXFONT,0                                                        
         LA    R1,H1               PRINT ADDRESSES FOR STANDARD                 
         ST    R1,AH1                                                           
         LA    R1,H4                                                            
         ST    R1,AH4                                                           
         LA    R1,P                                                             
         ST    R1,AP1                                                           
         MVC   PWIDTH,=F'132'                                                   
         LA    R1,REGSPECS                                                      
         ST    R1,SPECS                                                         
         CLI   NARROPT,C'Y'                                                     
         BNE   XIT                                                              
         LA    R1,NARSPECS                                                      
         ST    R1,SPECS                                                         
         B     XIT                                                              
         SPACE                                                                  
DRIWIDE  MVC   BOXWIDTH,=F'165'                                                 
         MVI   BOXFONT,1                                                        
         L     R4,BOXAWIDE                                                      
         USING WIDED,R4                                                         
         LA    R1,XHEAD1                                                        
         ST    R1,AH1                                                           
         LA    R1,XHEAD4                                                        
         ST    R1,AH4                                                           
         LA    R1,XP                                                            
         ST    R1,AP1                                                           
         MVC   PWIDTH,=F'198'                                                   
         LA    R1,WIDSPECS                                                      
         ST    R1,SPECS                                                         
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              OTHER DATA HANDLING ROUTINES                                     
         SPACE 3                                                                
VNUMERIC TM    4(R2),X'08'                                                      
         BO    VPACK                                                            
         MVI   ERROR,3                                                          
         B     VEXIT                                                            
         SPACE                                                                  
VPACK    SR    R3,R3                                                            
         IC    R3,5(R2)                                                         
         SR    R1,R1                                                            
         ZAP   DUB,=P'0'                                                        
         LTR   R3,R3                                                            
         BZ    XITR1                                                            
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R1,DUB                                                           
         SPACE                                                                  
ITSFINE  SR    R1,R1                                                            
         B     *+8                                                              
         SPACE                                                                  
NOGOOD   LA    R1,1                                                             
         LTR   R1,R1                                                            
         SPACE                                                                  
XITR1    XIT1  REGS=(R1)                                                        
         EJECT                                                                  
*              INSERT DELETE UNPROTECTED FIELDS                                 
         SPACE 3                                                                
*              INPUT               R2=A(FIRST UNPROTECTED FIELD)                
*                                  MAX=NUMBER OF INPUT FIELDS                   
         SPACE                                                                  
DELINS   NTR1                                                                   
         CLI   OFFLINE,C'Y'                                                     
         BE    XIT                                                              
         CLI   PFAID,3             WAS PF3 OR PF4 HIT                           
         BE    DI2                                                              
         CLI   PFAID,4                                                          
         BE    DI2                                                              
         CLI   PFAID,15            PF15 PF16 EQUIVALENT                         
         BE    DI2                                                              
         CLI   PFAID,16                                                         
         BNE   XIT                                                              
         SPACE                                                                  
DI2      L     R4,ATIOB                                                         
         USING TIOBD,R4                                                         
         LH    R4,TIOBCURD         PICK UP RELATIVE DISPLACEMENT                
         AR    R4,RA               INTO TWA                                     
         ZIC   R0,MAX                                                           
         SPACE                                                                  
DI4      CR    R2,R4                                                            
         BE    DI6                                                              
         BAS   RE,BUMPTOUN                                                      
         BCT   R0,DI4                                                           
         B     XIT                 (NOT IN THIS PART OF THE SCREEN)             
         SPACE                                                                  
DI6      CLI   PFAID,3                                                          
         BE    DEL2                                                             
         CLI   PFAID,15                                                         
         BE    DEL2                                                             
         XC    BLOCK(80),BLOCK                                                  
         SPACE                                                                  
INS2     MVC   BLOCK+80(80),8(R2)  SAVE THIS FIELD                              
         ZIC   R1,0(R2)            GET L'FIELD-1 INTO R1                        
         SH    R1,=H'9'                                                         
         TM    1(R2),X'02'                                                      
         BNO   *+8                                                              
         SH    R1,=H'8'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),BLOCK       MOVE IN PREVIOUS (OR CLEAR)                  
         OI    6(R2),X'80'                                                      
         MVC   BLOCK(80),BLOCK+80                                               
         BAS   RE,BUMPTOUN                                                      
         BCT   R0,INS2                                                          
         B     INSFOUND                                                         
         SPACE                                                                  
INSFOUND MVC   CONHEAD(L'INSMESS),INSMESS                                       
         L     R4,ATIOB                                                         
         LH    R2,TIOBCURD         PICK UP RELATIVE DISPLACEMENT                
         AR    R2,RA               INTO TWA                                     
         B     VERRX2                                                           
         SPACE                                                                  
DEL2     LR    R3,R2                                                            
         BAS   RE,BUMPTOUN                                                      
         CLI   5(R2),0                                                          
         BE    DEL4                                                             
         ZIC   R1,0(R3)            GET L'FIELD-1 INTO R1                        
         SH    R1,=H'9'                                                         
         TM    1(R3),X'02'                                                      
         BNO   *+8                                                              
         SH    R1,=H'8'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R3),8(R2)       MOVE NEXT INTO THIS                          
         OI    6(R3),X'80'                                                      
         BCT   R0,DEL2                                                          
         SPACE                                                                  
DEL4     EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R3),8(R3)       CLEAR LAST ONE                               
         OI    6(R3),X'80'                                                      
         LR    R2,R3                                                            
         MVC   CONHEAD(L'DELMESS),DELMESS                                       
         B     VERRX2                                                           
         DROP  R4                                                               
         EJECT                                                                  
*              POSITION CURSOR TO CORRECT FIELD IN ERRORS                       
         SPACE 3                                                                
*              INPUTS              R2=A(SCREEN HEADER)                          
*                                  FIELDERR=NUMBER OF FIELD IN ERROR            
         SPACE                                                                  
VCURSERR CLI   FIELDERR,0          APPLICATION MUST SET FIELD NUMBER            
         BE    VERRXIT                                                          
         CLI   OFFLINE,C'Y'                                                     
         BE    VERRXIT                                                          
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
         SPACE                                                                  
CURSERR2 CLI   0(RE),C','          SCAN FOR THE COMMAS                          
         BNE   CURSERR4                                                         
         BCT   RF,CURSERR4                                                      
         LA    R1,1(R1)            FOUND ENOUGH - SPACE PAST LAST               
         B     CURSERR6                                                         
         SPACE                                                                  
CURSERR4 LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,CURSERR2                                                      
         SR    R1,R1               ERROR - DIDN'T FIND ENOUGH COMMAS            
         SPACE                                                                  
CURSERR6 STC   R1,TIOBCURI         SET CURSOR DISPLACEMENT WITHIN FIELD         
         B     VERRXIT                                                          
         EJECT                                                                  
*              COMMON EXIT ROUTINES                                             
         SPACE 3                                                                
BUMP     ZIC   RF,0(R2)            GET TO NEXT SCREEN FIELD                     
         AR    R2,RF                                                            
         BR    RE                                                               
         SPACE                                                                  
BUMPTOUN ZIC   RF,0(R2)            GET TO NEXT UNPROTECTED FIELD                
         AR    R2,RF                                                            
         CLI   0(R2),0                                                          
         BER   RE                                                               
         TM    1(R2),X'20'                                                      
         BO    BUMPTOUN                                                         
         BR    RE                                                               
         SPACE                                                                  
FINV     MVI   ERROR,INVALID                                                    
         B     VERRXIT                                                          
         SPACE                                                                  
MYEND    MVI   ERROR,X'FE'                                                      
         B     VERRXIT                                                          
         SPACE                                                                  
MYCURSOR MVI   ERROR,X'FE'                                                      
         GOTO1 CURSERR                                                          
         SPACE                                                                  
VEXIT    DS    0H                                                               
VERRXIT  OI    6(R2),X'40'         POSITION CURSOR                              
         OI    CONHEADH+6,X'80'    ALWAYS TRANSMIT HEADER                       
         CLI   ERROR,X'FE'                                                      
         BE    VERRX2                                                           
         GOTO1 ERREX               SYSTEM MESSAGE                               
         SPACE                                                                  
VERRX2   GOTO1 ERREX2              MY OWN ERROR MESSAGE                         
         SPACE                                                                  
         EJECT                                                                  
*              CONSTANTS TABLES ETC                                             
         SPACE                                                                  
         SPACE                                                                  
RELO     DS    A                                                                
RELOC    DC    A(*)                                                             
         SPACE                                                                  
REGSPECS DS    0C                                                               
         SSPEC H1,2,AGYNAME                                                     
         SSPEC H2,2,REQUESTOR                                                   
*        SSPEC H2,97,AGYADD                                                     
         SSPEC H1,97,REPORT                                                     
         SSPEC H1,110,PAGE                                                      
         SSPEC H2,97,RUN           SPECS FOR REGULAR PRINTING                   
         DC    X'00'                                                            
         SPACE                                                                  
NARSPECS DS    0C                                                               
         SSPEC H1,2,AGYNAME                                                     
         SSPEC H2,2,REQUESTOR                                                   
         SSPEC H1,60,RUN           SPECS FOR NARROW PRINTING                    
         SSPEC H2,60,REQUESTOR                                                  
         SSPEC H4,60,REPORT                                                     
         SSPEC H4,73,PAGE                                                       
         DC    X'00'                                                            
         SPACE                                                                  
WIDSPECS DS    0C                                                               
         WSPEC H1,2,AGYNAME                                                     
         WSPEC H2,2,REQUESTOR                                                   
*        WSPEC H2,129,AGYADD                                                    
         WSPEC H1,129,REPORT                                                    
         WSPEC H1,142,PAGE                                                      
         WSPEC H2,129,RUN            SPECS FOR WIDE PRINTING                    
         DC    X'00'                                                            
         SPACE                                                                  
EDITLIST DC    XL64'00'            ONLY USED OFFLINE                            
         EJECT                                                                  
*              INITIALIZE SYSTEM ADDRESSES                                      
         SPACE                                                                  
SYSINIT  NTR1                                                                   
*                                  GET TERMINAL VALUES                          
         MVI   DDS,C'N'                                                         
         CLI   TWAOFFC,C'*'        TEST FOR DDS TERMINAL                        
         BNE   *+8                                                              
         MVI   DDS,C'Y'                                                         
         STM   R6,R7,BASER6                                                     
         MVC   TERM,TWATRM                                                      
         MVC   WRIAUTH,TWAAUTH                                                  
         MVC   USERID,TWAORIG                                                   
         MVC   AGYALPHA,TWAAGY                                                  
         MVC   AGENCY,TWAAGY                                                    
         MVI   FILTIDNO,10         PROGRAM FILTER FIELD ID 10                   
         MVI   TWANSAVE,0          OUTSMART GENCON - DON'T RESTORE              
         SPACE                                                                  
*                                  SET UP CERTAIN ADDRESSES                     
         L     R1,SYSPARMS                                                      
         MVC   ATIOB+1(3),1(R1)    P1 1-3 HAS A(TIOB)                           
         LM    R3,R4,12(R1)        A(TIA) A(COMFACS)                            
         ST    R4,ACOMFACS                                                      
         USING COMFACSD,R4                                                      
         ST    R3,ATIA                                                          
         MVC   CALLOV,CCALLOV                                                   
         MVC   DATAMGR,CDATAMGR                                                 
         MVC   GETMSG,CGETMSG                                                   
         MVC   HELLO,CHELLO                                                     
         MVC   SCANNER,CSCANNER                                                 
         MVC   DEMOVAL,CDEMOVAL                                                 
         MVC   SCANNER,CSCANNER                                                 
         SPACE                                                                  
         LA    R2,VCOMMON                                                       
         SR    R3,R3                                                            
         LA    R4,SYSCOMM                                                       
         LA    R5,NSYSCOMM                                                      
SYS10    ST    R2,0(R4)            SET COMMON BRANCH ADDRESSES                  
         STC   R3,0(R4)                                                         
         LA    R3,1(,R3)                                                        
         LA    R4,4(,R4)                                                        
         BCT   R5,SYS10                                                         
         SPACE                                                                  
* ALL GEN RTNS IN 00 PHASE FOR NOW - NOTE R2/R3 CARRIES ON                      
         SPACE                                                                  
         LA    R4,WRICOMM                                                       
         LA    R5,NWRICOMM                                                      
SYS30    ST    R2,0(R4)            SET COMMON BRANCH ADDRESSES                  
         STC   R3,0(R4)                                                         
         LA    R3,1(,R3)                                                        
         LA    R4,4(,R4)                                                        
         BCT   R5,SYS30                                                         
         SPACE                                                                  
         XC    DMCB,DMCB           OBTAIN CORE-RESIDENT ADDRESSES               
         LA    R2,CORETAB                                                       
         LA    R0,CORES            COUNTER                                      
         LA    R3,COREFACS         POINT TO ADDRESS AREA                        
         L     RF,CALLOV                                                        
         LA    R1,DMCB                                                          
         MVC   DMCB+4(3),=X'D9000A'                                             
         SPACE                                                                  
SYS20    MVC   DMCB+7(1),0(R2)                                                  
         GOTO1 (RF),(R1),0                                                      
         MVC   0(4,R3),DMCB        SAVE MODULE ADDRESS                          
         LA    R2,1(R2)            NEXT MODULE NUMBER                           
         LA    R3,4(R3)            NEXT ADDRESS                                 
         BCT   R0,SYS20                                                         
         SPACE                                                                  
*              OTHER INITIALIZATION                                             
         SPACE 3                                                                
*                                  SEED SYSD WITH DUMP COMMENTS                 
         SPACE                                                                  
         MVC   DUMPSYSD,=C'**SYSD**'                                            
         MVC   DUMPTLVL,=C'*REPVAL*'                                            
         MVC   DUMPRPGN,=C'*RERPGN*'                                            
         MVC   DUMPASRT,=C'*ASSORT*'                                            
         MVC   DUMPTLIO,=C'*SYSIOD*'                                            
         SPACE                                                                  
         LA    R1,BUFF                                                          
         MVC   0(8,R1),=C'**DPG***'                                             
         LA    R1,8(R1)                                                         
         ST    R1,ADPGPROG                                                      
         ST    R1,DRSTBUF                                                       
         LA    R1,3000(R1)                                                      
         ST    R1,DRENDBUF                                                      
         MVC   0(8,R1),=C'*DRONIO*'                                             
         LA    R1,8(R1)                                                         
         ST    R1,DRTALIO                                                       
         LA    R1,1000(R1)                                                      
         MVC   0(8,R1),=C'*COLFLT*'                                             
         LA    R1,8(R1)                                                         
         ST    R1,ACOLFILT                                                      
         SPACE                                                                  
*                                  SET SYSTEM DEPENDENT VALUES                  
         L     RF,=V(DUMMY)        END OF SYSTEM BASE                           
         A     RF,RELO                                                          
         ST    RF,SYSDUMMY                                                      
         MVI   SYSTEM,C'R'         REP                                          
         MVI   MAXIOS,2            USES 2 I/O AREAS                             
         MVC   SIZEIO,=F'1000'     EACH I/O IS 1000 BYTES                       
         MVC   GETUSER,WRIUSER     ROUTINE TO GET USER NAME AND ADDRESS         
         MVC   LKEY,=H'27'         DETAILS OF DIRECTORY AND KEY                 
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'34'                                                  
         MVC   SYSDIR,=C'REPDIR  '                                              
         MVC   SYSFIL,=C'REPFILE '                                              
         MVC   REQFILE,=C'REPREQ '                                              
         MVI   ACTELOPT,C'N'       NO ACTIVITY ELEMENTS                         
         MVI   GETMSYS,70          USES GETMSG FOR SYSTEM 70                    
         MVC   LWORK,=AL4(WKEND-WKST) WE TOOK XXXXX BYTES IN NMOD               
         MVC   RCPROG(2),=C'RE'    PREFIX FOR REPORT NO.                        
         MVC   SYSPHASE,=X'D9082100'    PRESET FOR SYSTEM CALLOVS               
         LA    RF,RECACTS            RECORD/ACTION DIRECTORY                    
         ST    RF,ARECACT            RECORD/ACTION DIRECTORY                    
         LA    R1,RGD                                                           
         ST    R1,ASTARTSV                                                      
         OI    GENSTAT1,RDUPAPPL   DON'T READ FOR UPDATE                        
         LA    R1,TVBLST                                                        
         ST    R1,REATVBLS                                                      
         SPACE                                                                  
SYSINTX  B     XIT                                                              
         SPACE                                                                  
         GETEL R4,DATADISP,ELCODE                                               
         SPACE                                                                  
REGERR   L     R1,=A(REGERRMS)                                                  
         B     ERRXIT2                                                          
OFFERR   L     R1,=A(OFFERRMS)                                                  
         B     ERRXIT2                                                          
PDTERR   L     R1,=A(PDTERMS)                                                   
         B     ERRXIT2                                                          
GRPERR   L     R1,=A(GRPERRMS)    VALIDATE GROUP/SUBGRP                         
         B     ERRXIT2                                                          
STAERR   L     R1,=A(STAERRMS)    VALIDATE STATION CALL LETTERS                 
         B     ERRXIT2                                                          
ACTERR   L     R1,=A(ACTERMS)                                                   
         B     ERRXIT2                                                          
SALERR   L     R1,=A(SALERRMS)                                                  
         B     ERRXIT2                                                          
SALOFFER L     R1,=A(SALOFFMS)                                                  
         B     ERRXIT2                                                          
DTERR    L     R1,=A(DTERMS)      VALIDATE DIVISION/TEAM                        
         B     ERRXIT2                                                          
ADVERR   L     R1,=A(ADVERMS)    VALIDATE ADVERTISER                            
         B     ERRXIT2                                                          
AGYERR   L     R1,=A(AGYERRMS)    VALIDATE AGENCY                               
         B     ERRXIT2                                                          
CLSERR   L     R1,=A(CLSERMS)     CLASS ERROR                                   
         B     ERRXIT2                                                          
CATERR   L     R1,=A(CATERMS)     VALIDATE CATEGORY                             
         B     ERRXIT2                                                          
MISADVER L     R1,=A(MISADVMS)    MISSING ADVERTISER FOR PRODUCT                
         B     ERRXIT2                                                          
PRDERR   L     R1,=A(PRDERMS)     PRODUCT                                       
         B     ERRXIT2                                                          
STYERR   L     R1,=A(STYERMS)     VALIDATE STATION TYPE                         
         B     ERRXIT2                                                          
LENERR   L     R1,=A(LENERMS)     INVALID LENGTH                                
         B     ERRXIT2                                                          
CTYPERR  L     R1,=A(CTYPERMS)    VALIDATE CONTRACT TYPE                        
         B     ERRXIT2                                                          
TVBERR   L     R1,=A(TVBERMS)     TVB ERROR                                     
         B     ERRXIT2                                                          
STOERR   L     R1,=A(STOERMS)     STATION OWNER NOT FOUND                       
         B     ERRXIT2                                                          
RNKERR   L     R1,=A(RNKERMS)     RANK ERROR                                    
         B     ERRXIT2                                                          
OFFLENER L     R1,=A(OFFLENMS)                                                  
         B     ERRXIT2                                                          
AOFLENER L     R1,=A(AOFLENMS)                                                  
         B     ERRXIT2                                                          
REPERR   L     R1,=A(REPERRMS)                                                  
         B     ERRXIT2                                                          
VTVBERR  L     R1,=A(TVBERRMS)                                                  
         B     ERRXIT2                                                          
INVDEMER L     R1,=A(INVDEM)              INVALID DEMO                          
         B     ERRXIT2                                                          
INVBOKER L     R1,=A(INVBOK)              INVALID BOOK                          
         B     ERRXIT2                                                          
MANYBKER L     R1,=A(MANYBKS)             TOO MANY BOOKS                        
         B     ERRXIT2                                                          
FHELP    L     R1,=A(FTRHPMS)                                                   
         B     ERRXIT2                                                          
VOFFSERR MVI   ERROR,SECLOCK      SECURITY LOCKOUT                              
         GOTO1 ERREX                                                            
         SPACE                                                                  
         SPACE                                                                  
ERRXIT2  A     R1,RELO                                                          
         MVC   CONHEAD,0(R1)                                                    
ERRXIT2A GOTO1 ERREX2                                                           
XIT      XIT1                                                                   
*                                  ERROR MESSAGES                               
         SPACE                                                                  
OPTERR   DC    C'* ERROR * INVALID OPTION'                                      
FTRERMS  DC    C'* ERROR * INVALID FILTER'                                      
ROWERR   DC    C'* ERROR * INVALID ROW EXPRESSION'                              
ROWONLY  DC    C'* ERROR * NOT ALLOWED AS A COLUMN'                             
COLERR   DC    C'* ERROR * INVALID COLUMN EXPRESSION'                           
USRERR   DC    C'* ERROR * CANT FIND USER RECORD'                               
COLONLY  DC    C'* ERROR * NOT ALLOWED AS A ROW'                                
HOVERR   DC    C'HEADING OVERRIDE IS WIDER THAN COLUMN'                         
PEXERR   DC    C'PERIOD EXPRESSION INCONSISTENT WITH REQUEST'                   
NEED1ERR DC    C'MUST BE AT LEAST 1 ROW AND 1 COLUMN'                           
LRANKERR DC    C'NOT VALID TO RANK ON LAST ROW'                                 
INSMESS  DC    C'NEW FIELD INSERTED ON SCREEN'                                  
DELMESS  DC    C'FIELD DELETED ON SCREEN'                                       
         EJECT                                                                  
*              CONSTANTS TABLES ETC                                             
         SPACE 3                                                                
CORETAB  DS    0C                  TABLE OF CORE-RESIDENT MODULES               
         DC    AL1(QGENCON)        GENCON                                       
         DC    AL1(QDRONE)         DRONE                                        
         DC    AL1(QQSORT)         QSORT                                        
         DC    AL1(QGETBROD)       GET BROAD CAST DATES                         
         DC    AL1(QDEMOVAL)                                                    
         DC    AL1(QDEMOCON)                                                    
         DC    AL1(QREFETCH)                                                    
CORES    EQU   (*-CORETAB)                                                      
         SPACE                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              TABLES OF RECORDS ACTIONS AND COMBINATIONS                       
         SPACE 3                                                                
RECACTS  DS    0D                                                               
         SPACE                                                                  
* X'01' ENTRIES ARE AVAILABLE REPORTS                                           
*                                                                               
* CL8 EXPANDED REPORT NAME                                                      
* CL1 REPORT NUMBER (01 IS RESERVED)                                            
* CL1 PHASE NUMBER FOR DATA DICTIONARY - LEAVE AS ZERO FOR NOW                  
* CL1 PHASE NUMBER FOR HELP SCREEN - LEAVE AS ZERO FOR NOW                      
         SPACE                                                                  
****     DC    X'04',C'WRITER  ',AL1(01),X'0000'                                
         DC    X'04',C'AUR     ',AL1(02),X'0000'                                
         DC    X'04',C'TEST    ',AL1(03),X'0000'                                
         SPACE 3                                                                
* X'02' ENTRIES ARE AVAILABLE ACTIONS                                           
*                                                                               
* CL8 EXPANDED ACTION NAME                                                      
* CL1 ACTION NUMBER                                                             
* CL1 ACTION EQUATE                                                             
* CL1 SPARE                                                                     
         SPACE                                                                  
         DC    X'02',C'ADD     ',AL1(01,01,00)                                  
         DC    X'02',C'CHANGE  ',AL1(02,01,00)                                  
         DC    X'02',C'CREATE  ',AL1(12,13,00)                                  
         DC    X'02',C'DISPLAY ',AL1(03,01,00)                                  
         DC    X'02',C'DELETE  ',AL1(04,01,00)                                  
         DC    X'02',C'SELECT  ',AL1(05,01,00)                                  
         DC    X'02',C'REPORT  ',AL1(12,12,00)                                  
         DC    X'02',C'RESTORE ',AL1(06,01,00)                                  
         DC    X'02',C'LIST    ',AL1(10,10,00)                                  
         EJECT                                                                  
* DIRECTORY OF PHASES FOR SELECTED RECORD/ACTION                                
         SPACE 3                                                                
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
         SPACE                                                                  
*                                 SC  SP  OK                                    
*                                   OV  RP                                      
*****    DC    X'03',AL1(01,12),X'F101000138',C'WROW'  REPORT WRITER            
         DC    X'03',AL1(02,12),X'F020002038',C'WRAU'  AUR REPORT               
         DC    X'03',AL1(03,12),X'F021002138',C'WRAU'  AUR TEST                 
         DC    X'FF'                                                            
         SPACE 3                                                                
*              PHASE USED UP BY SYSTEM SO FAR                                   
         SPACE                                                                  
*              X0 X1 X2 X3 X4 X5 X6 X7 X8 X9 XA XB XC XD XE XF                  
*                                                                               
*        0X     Y  Y                                                            
*        1X                                                                     
*        FX        Y                                                            
REPERRMS DC    CL60'* ERROR * NO REP REC FOUND *'                               
TVBERRMS DC    CL60'* ERROR * NO TVB FOUND *'                                   
OFFERRMS DC    CL60'* ERROR * NO OFFICE FOUND *'                                
OFFLENMS DC    CL60'* ERROR * OFFICE MUST BE 1-4 CHARACTERS *'                  
AOFLENMS DC    CL60'* ERROR * AGENCY OFFICE MUST BE 1-2 CHARACTERS *'           
PDTERMS  DC    CL60'* ERROR * INVALID PERIOD - ENTER MONYR  OR MONYR-MOC        
               NYR *'                                                           
GRPERRMS DC    CL60'* ERROR * NO GROUP/SUBGROUP FOUND *'                        
STAERRMS DC    CL60'* ERROR * NO STATION FOUND *'                               
ACTERMS  DC    CL60'* ERROR * BAD ACTIVITY DATES *'                             
SALERRMS DC    CL60'* ERROR * NO SALESPERSON FOUND *'                           
SALOFFMS DC    CL60'* ERROR * SALESPERSON NOT IN OFFICE *'                      
DTERMS   DC    CL60'* ERROR * NO DIVISION/TEAM FOUND *'                         
ADVERMS  DC    CL60'* ERROR * NO ADVERTISER FOUND *'                            
AGYERRMS DC    CL60'* ERROR * NO AGENCY FOUND *'                                
CLSERMS  DC    CL60'* ERROR * BAD CLASS *'                                      
CATERMS  DC    CL60'* ERROR * NO CATEGORY FOUND *'                              
MISADVMS DC    CL60'* ERROR * ADVERTISER MUST BE ENTERED FOR PRODUCT *'         
PRDERMS  DC    CL60'* ERROR * NO PRODUCT FOUND FOR ADVERTISER *'                
STYERMS  DC    CL60'* ERROR * NO STATION TYPE FOUND *'                          
LENERMS  DC    CL60'* ERROR * INVALID LENGTH *'                                 
CTYPERMS DC    CL60'* ERROR * NO CONTRACT TYPE FOUND *'                         
TVBERMS  DC    CL60'* ERROR * NO TVB FOUND *'                                   
STOERMS  DC    CL60'* ERROR * NO STATION OWNER FOUND *'                         
RNKERMS  DC    CL60'* ERROR * RANK MUST BE 0-9 *'                               
REGERRMS DC    CL60'* ERROR * NO REGION FOUND *'                                
INVDEM   DC    CL60'* ERROR * INVALID DEMO'                                     
INVBOK   DC    CL60'* ERROR * INVALID BOOK'                                     
MANYBKS  DC    CL60'* ERROR * TOO MANY BOOKS - LIMIT IS 1'                      
FTRHPMS  DC    CL60'FILTERS=CONTYPE/OWNER/RANK/TVB/DEMO/BOOK *'                 
         EJECT                                                                  
* DISPLAY FILTER  R0 = START OF AREA                                            
*                 R1 = CURR PTR IN AREA                                         
*                 R2 = LENGTH OF DESCRIPTION                                    
*                 R3 = CURR DATA ADDR                                           
*                 R4 = LENGTH OF DATA - 1                                       
*                 WORK = DESCRIPTION                                            
         SPACE                                                                  
DFTR     NTR1  BASE=*,LABEL=*                                                   
         LA    R2,FILTABL                                                       
         LA    R3,BLOCK                                                         
         ST    R3,DUB                                                           
*                                                                               
* HARD CODE ADDED TO MAKE SF BOOK FILTER PRINT                                  
         OC    REFBOOK,REFBOOK                                                  
         BZ    DFTR10                                                           
         GOTO1 =V(UNBOOK),DMCB,(1,REFBOOK),WORK                                 
         LA    RE,WORK+15+8                                                     
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
*                                                                               
         CLI   REFBOOK+3,0                                                      
         BE    *+16                                                             
         MVC   1(3,RE),=C'( )'                                                  
         MVC   2(1,RE),REFBOOK+3                                                
*&&DO                                                                           
         CLI   0(RE),C')'                                                       
         BNE   DFTR5                                                            
         BCTR  RE,0                                                             
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
         CLI   0(RE),C'('                                                       
         BNE   *+10                                                             
         BCTR  RE,0                                                             
         B     DFTR5                                                            
*                                                                               
         LA    RE,1(RE)                                                         
         MVI   0(RE),C')'                                                       
*&&                                                                             
DFTR5    DS    0H                                                               
         MVC   0(5,R3),=C'BOOK='                                                
         MVC   5(15,R3),WORK+8                                                  
         LA    R3,20(R3)                                                        
         B     DFTR15                                                           
         SPACE                                                                  
DFTR10   CLI   0(R2),X'FF'         AT END OF TABLE                              
         BE    DFTRX                YES                                         
         SPACE                                                                  
         ZIC   RE,2(R2)            LENGTH OF FIELD                              
         SR    RF,RF                                                            
         ICM   RF,3,0(R2)          ADDR OF FIELD                                
         LA    RF,REFILTS(RF)                                                   
         SPACE                                                                  
         EX    RE,DFTROC           FILTERING ON IT                              
         BZ    DFTR20               NO                                          
         SPACE                                                                  
         C     R3,DUB              PREVIOUS FILTERS                             
         BE    *+12                                                             
         MVI   0(R3),C','                                                       
         LA    R3,1(,R3)                                                        
         SPACE                                                                  
         ZIC   R1,11(R2)           LENGTH OF FILTER DESC                        
         EX    R1,DFTRMVCA                                                      
         LA    R3,1(R1,R3)                                                      
         EX    RE,DFTRMVCB                                                      
         SPACE                                                                  
         LA    R3,1(RE,R3)                                                      
DFTR15   CLI   0(R3),C' '                                                       
         BH    *+8                                                              
         BCT   R3,*-8                                                           
         LA    R3,1(,R3)                                                        
         SPACE                                                                  
         CLC   FILTCONT,3(R2)      IF CONTRACT TYPE, CK EXCLUDE                 
         BNE   DFTR20                                                           
         CLI   REFCTYEX,C'*'       EXCLUDE THIS TYPE                            
         BNE   DFTR20               NO                                          
         BCTR  R3,0                                                             
         MVC   0(1,R3),REFCTYEX                                                 
         MVC   1(1,R3),REFCTY                                                   
         LA    R3,2(,R3)                                                        
         SPACE                                                                  
DFTR20   LA    R2,12(,R2)                                                       
         B     DFTR10                                                           
DFTRX    B     XIT                                                              
DFTROC   OC    0(0,RF),0(RF)                                                    
DFTRMVCA MVC   0(0,R3),3(R2)                                                    
DFTRMVCB MVC   0(0,R3),0(RF)                                                    
*                                  FILTER AREAS AND DESCRIPTIONS                
         SPACE                                                                  
FILTABL  DC    AL2(REFREG-REFILTS),AL1(L'REFREG-1),CL8'REGION= ',AL1(6)         
         DC    AL2(REFOFF-REFILTS),AL1(L'REFOFF-1),CL8'OFFICE= ',AL1(6)         
         DC    AL2(REFGRPSB-REFILTS),AL1(L'REFGRPSB-1),CL8'GROUP= '             
         DC    AL1(5)                                                           
         DC    AL2(REFSTA-REFILTS),AL1(L'REFSTA-1),CL8'STATION=',AL1(7)         
         DC    AL2(REFSAL-REFILTS),AL1(L'REFSAL-1),CL8'SAL=    ',AL1(3)         
         DC    AL2(REFDT-REFILTS),AL1(L'REFDT-1),CL8'DIV=      ',AL1(3)         
         DC    AL2(REFADV-REFILTS),AL1(L'REFADV-1),CL8'ADV=    ',AL1(3)         
         DC    AL2(REFAGY-REFILTS),AL1(L'REFAGY-1),CL8'AGY=    ',AL1(3)         
         DC    AL2(REFCLS-REFILTS),AL1(L'REFCLS-1),CL8'CLASS=  ',AL1(5)         
         DC    AL2(REFCTG-REFILTS),AL1(L'REFCTG-1),CL8'CAT=    ',AL1(3)         
         DC    AL2(REFPRD-REFILTS),AL1(L'REFPRD-1),CL8'PRD=    ',AL1(3)         
         DC    AL2(REFCTY-REFILTS),AL1(L'REFCTY-1)                              
FILTCONT DC    CL8'CONTYPE=',AL1(7)                                             
         DC    AL2(REFRNK-REFILTS),AL1(L'REFRNK-1),CL8'RANK=   ',AL1(4)         
         DC    AL2(REFOWN-REFILTS),AL1(L'REFOWN-1),CL8'OWN=    ',AL1(3)         
         DC    AL2(REFPRD-REFILTS),AL1(L'REFPRD-1),CL8'PRD=    ',AL1(3)         
         DC    AL2(REFDEMO-REFILTS),AL1(L'REFDEMO-1),CL8'DEMO= ',AL1(4)         
*        DC    AL2(REFBOOK-REFILTS),AL1(L'REFBOOK-1),CL8'BOOK= ',AL1(4)         
         DC    AL2(REFLENE-REFILTS),AL1(L'REFLENE-1),CL8'LEN=  ',AL1(3)         
         DC    X'FF'                                                            
         EJECT                                                                  
*              DSECT TO DEFINE WORKING STORAGE                                  
WKLND    DSECT                                                                  
WKST     DS    CL(SPOOLEND-SPOOLD)                                              
         DS    CL((GENDEND+16)-GEND)                                            
         DS    CL1000              (SPACE FOR SECOND I/O)                       
         DS    CL(SYSEND-WRISYS)                                                
WKEND    EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE REWRIWORKD                                                     
         EJECT                                                                  
*              OTHER DSECTS ARE HIDDEN IN HERE                                  
         SPACE 3                                                                
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*REWRIFFD                                                                       
*DDGENTWA                                                                       
*FAFACTS                                                                        
*FATIOB                                                                         
*DDCOMFACS                                                                      
*DDCOREQUS                                                                      
*DRGLOBAL                                                                       
*DRDICFILE                                                                      
*DDBIGBOX                                                                       
*DDWIDED                                                                        
*CTGENFILE                                                                      
*REGENALL1                                                                      
*REGENPWC                                                                       
*        PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE REWRIFFD                                                       
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FATIOB                                                         
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE DRGLOBAL                                                       
       ++INCLUDE DRDICD                                                         
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDWIDED                                                        
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
       ++INCLUDE REGENALL1                                                      
         EJECT                                                                  
       ++INCLUDE REGENPWC                                                       
         EJECT                                                                  
       ++INCLUDE DDPERVALD                                                      
         PRINT ON                                                               
         SPACE                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'053REWRI0052 05/01/02'                                      
         END                                                                    
