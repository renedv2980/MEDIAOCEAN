*          DATA SET SPSFM41    AT LEVEL 067 AS OF 05/01/02                      
*PHASE T21741A                                                                  
*                                                                               
         TITLE 'SPSFM41 BUYING RULES MAINT'                                     
T21741   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T21741*,R7                                                    
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         XC    ERRDISP,ERRDISP                                                  
         LA    R2,SRUTYPEH         FAKE OUT GENCON                              
         ST    R2,AFRSTREC                                                      
         OI    GENSTAT1,RDUPAPPL                                                
         OI    GLSTSTAT,APPLCDSP+CHNGLIST+NOSELFLD                              
*                                                                               
         CLI   MODE,VALKEY         FIRST TIME IN                                
         BNE   *+12                                                             
         BAS   RE,VK                                                            
         B     MAINX                                                            
*                                                                               
         CLI   MODE,LVALREC        VALIDATE LISTED RECORD                       
         BNE   *+12                                                             
         BAS   RE,LVREC                                                         
         B     MAINX                                                            
*                                                                               
         CLI   MODE,LISTRECS                                                    
         BNE   MAIN30                                                           
         MVC   LISTAR,SPACES       CLEAR PREVIOUS LINE                          
         B     MAIN40                                                           
*                                                                               
MAIN30   CLI   MODE,PRINTREP                                                    
         BNE   MAINX                                                            
         XC    KEY,KEY             ENSURE START AT TOP OF LIST                  
         ZAP   COUNTER,=P'0'       CLEAR LINE COUNTER                           
         LA    R2,MYSPECS                                                       
         ST    R2,SPECS                                                         
*                                                                               
MAIN40   BAS   RE,LREC             GO LIST THE RECORDS                          
*                                                                               
MAINX    B     EXIT                                                             
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
EXIT     XIT1                                                                   
         EJECT                                                                  
*                                                                               
*        VALIDATE KEY ROUTINE                                                   
*                                                                               
VK       NTR1                                                                   
         MVI   KEYCHG,C'N'                                                      
         MVC   LLIST,=Y(LLNQ)      SET L'LIST LINE                              
         MVI   NLISTS,13                                                        
         LA    R2,SRUMEDH          MEDIA                                        
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED                         
         BO    VK10                                                             
         MVI   KEYCHG,C'Y'                                                      
         GOTO1 VALIMED                                                          
         NI    SRUCLTH+4,X'DF'                                                  
*                                                                               
VK10     OI    4(R2),X'20'         VALIDATED                                    
         LA    R2,SRUCLTH          CLIENT                                       
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED                         
         BO    VK20                                                             
         MVI   KEYCHG,C'Y'                                                      
         GOTO1 VALICLT                                                          
         NI    SRUPRDH+4,X'DF'                                                  
*                                                                               
VK20     OI    4(R2),X'20'         VALIDATED                                    
         LA    R2,SRUPRDH          PRODUCT/GROUP                                
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED                         
         BO    VK30                                                             
         MVI   KEYCHG,C'Y'                                                      
         XC    SVPGRP,SVPGRP                                                    
         MVI   BPRD,0                                                           
         CLI   5(R2),0                                                          
         BE    VK30                                                             
         NI    SRUESTH+4,X'DF'                                                  
         CLI   5(R2),3                                                          
         BNH   VK25                                                             
         BAS   RE,VALPGR           THEN IT MUST BE PRODUCT GROUP                
         B     VK30                                                             
*                                                                               
VK25     GOTO1 VALIPRD                                                          
*                                                                               
VK30     OI    4(R2),X'20'         VALIDATED                                    
         LA    R2,SRUESTH          ESTIMATE                                     
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED                         
         BO    VK40                                                             
         MVI   KEYCHG,C'Y'                                                      
         MVI   BEST,0                                                           
         CLI   5(R2),0                                                          
         BE    VK40                                                             
         GOTO1 VALIEST                                                          
         NI    SRUDPTH+4,X'DF'                                                  
*                                                                               
VK40     OI    4(R2),X'20'         VALIDATED                                    
         LA    R2,SRUDPTH          DAYPART                                      
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED                         
         BO    VK50                                                             
         MVI   SVDPT,0                                                          
         MVI   KEYCHG,C'Y'                                                      
         MVC   SVDPT,8(R2)                                                      
         NI    SRUSTRTH+4,X'DF'                                                 
*                                                                               
VK50     OI    4(R2),X'20'         VALIDATED                                    
         LA    R2,SRUSTRTH         START AT                                     
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED                         
         BO    VK60                                                             
         MVI   KEYCHG,C'Y'                                                      
         MVI   SVRULE,0                                                         
         CLI   5(R2),0                                                          
         BE    VK60                                                             
         BAS   RE,FNDTYPE                                                       
         MVC   SVRULE,BYTE                                                      
*                                                                               
VK60     OI    4(R2),X'20'         VALIDATED                                    
         CLI   KEYCHG,C'Y'                                                      
         BNE   VKX                                                              
         BAS   RE,SETKEY                                                        
*                                                                               
VKX      B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*              ROUTINE VALIDATES PRODUCT GROUP                                  
*                                                                               
VALPGR   NTR1                                                                   
         CLC   =C'PGR=',8(R2)                                                   
         BNE   INVERR                                                           
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING PRGRECD,R3                                                       
         MVC   PRGKTYP,=X'0D01'    RECORD TYPE                                  
         MVC   PRGKAGMD,BAGYMD     AGY/MEDIA                                    
         MVC   PRGKCLT,BCLT        CLIENT                                       
         MVC   PRGKID,12(R2)       PRODUCT GROUP ID                             
         ZIC   R5,5(R2)                                                         
         SH    R5,=H'5'                                                         
         LTR   R5,R5                                                            
         BZ    VP20                                                             
         CH    R5,=H'3'            MAXIMUM OF 3 DIGITS                          
         BH    INVERR                                                           
*                                                                               
         LA    R4,13(R2)           POINT R4 TO START OF DIGITS                  
* MAY NOT ENTER ALL 9'S                                                         
         CLC   =C'999',0(R4)                                                    
         BE    INVERR                                                           
         STM   R4,R5,WORK          SAVE R4/R5                                   
*                                                                               
VP10     CLI   0(R4),C'0'          ENSURE ALL DIGITS                            
         BL    INVERR                                                           
         CLI   0(R4),C'9'                                                       
         BH    INVERR                                                           
         LA    R4,1(R4)                                                         
         BCT   R5,VP10                                                          
*                                                                               
         LM    R4,R5,WORK          RESTORE R4/R5                                
         XC    WORK,WORK                                                        
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(R4) *EXECUTED*                                         
         PACK  FULL+2(2),WORK(3)   GET DIGITS LEFT ALIGNED                      
         NI    FULL+3,X'F0'                                                     
         MVC   PRGKGRP,FULL+2                                                   
*                                                                               
VP20     GOTO1 HIGH                                                             
         CLC   KEY(L'PRGKEY),KEYSAVE                                            
         BNE   INVERR                                                           
         MVC   SVPGRP,PRGKID       SAVE ID/GRP NUMBER                           
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*              ROUTINE VALIDATES LISTED RECORDS                                 
*                                                                               
LVREC    NTR1                                                                   
         L     R3,ATHISLST         R3=A(THIS SCREEN LINE)                       
         USING LINED,R3                                                         
         L     R4,AIO                                                           
         USING RULRECD,R4                                                       
         CLC   PREVRUL,RULKRUL     IF SAME RULE RECORD                          
         BE    LVR10               CONTINUE                                     
         MVC   PREVRUL,RULKRUL                                                  
         MVI   ELCODE,X'05'        ELSE REMOVE OLD DATA ELEMENTS                
         GOTO1 REMELEM                                                          
*                                                                               
LVR10    LA    R2,LDATAH                                                        
         TM    4(R2),X'80'         HAS FIELD BEEN INPUTTED THIS TIME            
         BNO   LVR15                                                            
         MVC   MYSVKEY,FIRSTKEY                                                 
         MVI   REDISP,C'Y'                                                      
*                                                                               
***15    CLI   5(R2),0             GENCON WON'T CALL ME IF NO INPUT             
***      BE    MISSERR             (VALL20)                                     
LVR15    L     R5,AIO2                                                          
*                                                                               
         XC    0(32,R5),0(R5)      CLEAR 1ST ENTRY                              
         GOTO1 SCANNER,DMCB,0(R2),(R5)                                          
         CLI   4(R1),0                                                          
         BE    INVERR                                                           
         ZIC   R3,4(R1)                                                         
*                                                                               
LVR20    CLI   0(R5),0                                                          
         BE    INVERR                                                           
         BAS   RE,ADDRULES                                                      
         LA    R5,32(R5)           BUMP TO NEXT SCANNER ENTRY                   
         BCT   R3,LVR20                                                         
         OI    4(R2),X'20'                                                      
*                                                                               
LVRX     B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
*              ROUTINE CONTROLS RECORD LISTING                                  
*                                                                               
LREC     NTR1                                                                   
         LA    R4,KEY                                                           
         USING RULRECD,R4                                                       
         LA    R2,SRUADDH                                                       
         CLI   5(R2),0             ADDING A NEW ONE                             
         BE    LR40                YES                                          
         MVI   REDISP,C'Y'         RE-DISPLAY PAGE                              
         BAS   RE,FNDTYPE          YES - FIND CORRECT TYPE                      
         MVC   RULKRUL,BYTE        SET RULE TYPE IN KEY                         
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     RECORD FOUND                                 
         BE    *+8                                                              
         BAS   RE,ADDNEW           GO ADD NEW RECORD                            
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         LA    R2,SRUNDATH                                                      
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
         CLC   =C'DELETE',8(R2)                                                 
         BNE   LR10                                                             
         MVI   ELCODE,X'05'        REMOVE OLD DATA ELEMENTS                     
         GOTO1 REMELEM                                                          
         B     LR30                                                             
*                                                                               
LR10     L     R5,AIO2                                                          
         XC    0(32,R5),0(R5)      CLEAR 1ST ENTRY                              
         GOTO1 SCANNER,DMCB,0(R2),(R5)                                          
         CLI   4(R1),0                                                          
         BE    INVERR                                                           
         ZIC   R3,4(R1)                                                         
*                                                                               
LR20     CLI   0(R5),0                                                          
         BE    INVERR                                                           
         BAS   RE,ADDRULES                                                      
         LA    R5,32(R5)           BUMP TO NEXT SCANNER ENTRY                   
         BCT   R3,LR20                                                          
*                                                                               
LR30     GOTO1 PUTREC                                                           
         BAS   RE,SETKEY                                                        
         MVC   SRUADD,SPACES       CLEAR AFTER ADDING                           
         MVC   SRUNDAT,SPACES                                                   
         OI    SRUADDH+6,X'80'                                                  
         OI    SRUNDATH+6,X'80'                                                 
*                                                                               
LR40     DS    0H                                                               
         LA    R2,SRUTYPEH                                                      
         MVC   KEY,MYSVKEY         RECALL WHAT WE'RE LOOKING FOR                
         CLI   REDISP,C'Y'         RE-DISPLAY PAGE                              
         BE    LR45                                                             
         OC    LASTKEY,LASTKEY                                                  
         BZ    *+10                                                             
         MVC   KEY,LASTKEY                                                      
*                                                                               
LR45     GOTO1 HIGH                                                             
         MVC   FIRSTKEY,KEY        SAVE FIRST KEY READ ON SCREEN                
         MVI   REDISP,C'N'                                                      
         B     LR60                                                             
*                                                                               
LR50     GOTO1 SEQ                                                              
*                                                                               
LR60     CLC   KEY(10),MYSVKEY                                                  
         BNE   LR80                                                             
         MVC   MYSVKEY,KEY                                                      
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         BAS   RE,DISPLAY                                                       
         CLI   MODE,LISTRECS       IF LISTING ON SCREEN                         
         BNE   LR70                                                             
         AH    R2,=Y(LINNEXT)      PT TO NEXT AVAILABLE LINE                    
*                                                                               
LR70     CLI   MODE,PRINTREP       IF PRINTING REPORT                           
         BNE   LR50                                                             
         CP    COUNTER,=P'0'       AND IF ANYTHING REPORTED                     
         BE    LRX                                                              
         GOTO1 SPOOL,DMCB,(R8)     SKIP LINE BEFORE TOTAL                       
         EDIT  COUNTER,(4,P),COMMAS=YES,ALIGN=LEFT                              
         LR    R1,R0                                                            
         LA    R1,P+1(R1)                                                       
         MVC   0(12,R1),=C'RULE RECORDS'                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     LR50                                                             
*                                                                               
LR80     TM    WHEN,X'40'          IF SPOOLING AND NOW                          
         BZ    LRX                                                              
         XC    CONSERV,CONSERV     AUTO $DQU                                    
         MVC   CONSERV(4),=C'$DQU'                                              
*                                                                               
LRX      BAS   RE,SETKEY                                                        
         MVI   PREVRUL,0                                                        
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*        BUILD KEY                                                              
*                                                                               
SETKEY   NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RULRECD,R4                                                       
         MVC   RULKTYP,=X'0D92'                                                 
         MVC   RULKAGMD,BAGYMD     AGY/MEDIA                                    
         MVC   RULKCLT,BCLT        CLIENT                                       
         OC    SVPGRP,SVPGRP       IS THERE A PRODUCT GROUP                     
         BZ    SK10                                                             
         MVC   RULKPGCD(3),SVPGRP  PRODUCT GROUP                                
         B     SK20                                                             
*                                                                               
SK10     MVC   RULKPRD,BPRD        PRODUCT                                      
*                                                                               
SK20     MVC   RULKEST,BEST        ESTIMATE                                     
         MVC   RULKDPT,SVDPT       DAYPART                                      
         MVC   RULKRUL,SVRULE      RULE                                         
         MVC   MYSVKEY,KEY                                                      
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*        FIND RULE TYPE CODE IN TABLE                                           
*        R2 - FIELD HEADER                                                      
*                                                                               
FNDTYPE  NTR1                                                                   
         LA    R1,RULETAB                                                       
         MVC   WORK,SPACES                                                      
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
*                                                                               
FT10     CLI   0(R1),X'FF'                                                      
         BE    INVERR                                                           
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R1),8(R2)                                                    
         BE    FT20                                                             
         LA    R1,L'RULETAB(R1)                                                 
         B     FT10                                                             
*                                                                               
FT20     MVC   BYTE,10(R1)        SET RULE TYPE IN KEY                          
         MVC   WORK(10),0(R1)                                                   
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*              DISPLAY RECORD IN AIO                                            
*                                                                               
DISPLAY  NTR1                                                                   
         MVI   DRYRUN,C'Y'                                                      
*                                                                               
D05      MVC   LINECNT,LISTNUM     WHERE WE CURRENTLY ARE                       
         L     R3,ATHISLST                                                      
         USING LINED,R3            R3 = A(OUTPUT AREA)                          
         CLI   DRYRUN,C'N'         IF NOT DRY RUN - DON'T KEEP LAST KEY         
         BE    *+10                                                             
         MVC   LASTKEY,KEY         SAVE LAST KEY                                
         L     R4,AIO                                                           
         MVI   ELCODE,5            SEE IF ANY ELEMENTS TO LIST                  
         BAS   RE,GETEL                                                         
         BNE   D50                                                              
*                                                                               
         L     R4,AIO                                                           
         USING RULRECD,R4                                                       
*                                                                               
         CLI   DRYRUN,C'Y'         IS DRY RUN - DON'T DISPLAY                   
         BE    D07                                                              
         MVC   BYTE,RULKRUL                                                     
         BAS   RE,FNDRULE          GET RULE NAME                                
         MVC   LTYPE,WORK                                                       
         OI    LTYPEH+6,X'80'      TRANSMIT                                     
         MVC   SVRULNM,WORK                                                     
*                                                                               
D07      LA    R5,LTYPEH                                                        
         MVC   LDATA,SPACES                                                     
         NI    LDATAH+4,X'FF'-X'20'   FORCE GENCON TO THINK THERE               
         LA    R2,LDATA                                                         
         LA    R6,L'LDATA(R2)         WAS A CHANGE                              
         L     R4,AIO                                                           
         USING RULDATAD,R4                                                      
         MVI   ELCODE,5                                                         
         BAS   RE,GETEL                                                         
         B     D20                                                              
*                                                                               
D10      BAS   RE,NEXTEL                                                        
*                                                                               
D20      BNE   D30                                                              
         ZIC   R1,RULDLEND         L'DATA                                       
         AR    R1,R2                                                            
         CR    R1,R6               STILL ENOUGH ROOM ON THIS LINE               
         BNL   D25                 NO SKIP TO NEXT                              
*                                                                               
D22      ZIC   R1,RULDLEND         L'DATA                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),RULDDATA                                                 
*                                                                               
         CLI   DRYRUN,C'Y'         IS DRY RUN - DON'T DISPLAY                   
         BNE   D23                                                              
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),SPACES                                                   
         AR    R2,R1                                                            
         LA    R2,2(R2)                                                         
         B     D10                                                              
*                                                                               
D23      AR    R2,R1                                                            
         MVI   1(R2),C','                                                       
         LA    R2,2(R2)                                                         
         B     D10                                                              
*                                                                               
D25      BCTR  R2,0                                                             
         CLI   0(R2),C','          CLEAR TRAILING COMMA                         
         BNE   *+8                                                              
         MVI   0(R2),C' '                                                       
         CLI   DRYRUN,C'Y'         IS DRY RUN - DON'T DISPLAY                   
         BE    *+8                                                              
         OI    LDATAH+6,X'80'      TRANSMIT                                     
         CLI   MODE,LISTRECS       IF LISTING ON SCREEN                         
         BNE   D27                                                              
         ZIC   R1,LINECNT                                                       
         LA    R1,1(R1)                                                         
         STC   R1,LINECNT                                                       
         CLI   DRYRUN,C'Y'         IS DRY RUN - DON'T DISPLAY                   
         BE    D27                                                              
         GOTO1 LISTMON                                                          
*                                                                               
D27      L     R3,ATHISLST                                                      
         LA    R5,LTYPEH                                                        
         CLI   DRYRUN,C'Y'         IS DRY RUN - DON'T DISPLAY                   
         BE    D29                                                              
         MVC   LTYPE,SVRULNM                                                    
         OI    LTYPEH+6,X'80'                                                   
         NI    LDATAH+4,X'FF'-X'20'   FORCE GENCON TO THINK THERE               
*                                           WAS A CHANGE                        
D29      LA    R2,LDATA                                                         
         MVC   LDATA,SPACES                                                     
         LA    R6,L'LDATA(R2)                                                   
         B     D22                                                              
*                                                                               
D30      BCTR  R2,0                                                             
         CLI   0(R2),C','          CLEAR TRAILING COMMA                         
         BNE   D40                                                              
         MVI   0(R2),C' '                                                       
         CLI   DRYRUN,C'Y'         IS DRY RUN - DON'T DISPLAY                   
         BE    *+8                                                              
         OI    LDATAH+6,X'80'      TRANSMIT                                     
*                                                                               
D40      CLI   MODE,LISTRECS       IF LISTING ON SCREEN                         
         BNE   D50                                                              
         ZIC   R1,LINECNT                                                       
         LA    R1,1(R1)                                                         
         STC   R1,LINECNT                                                       
         CLI   DRYRUN,C'Y'         IS DRY RUN - DON'T DISPLAY                   
         BE    D50                                                              
         GOTO1 LISTMON                                                          
*                                                                               
D50      CLI   DRYRUN,C'N'                                                      
         BE    DX                                                               
         CLC   LINECNT,NLISTS      IS THERE ENOUGH ROOM FOR THIS RULE           
         BH    D60                                                              
         MVI   DRYRUN,C'N'         YES - SET FLAG TO GO TO LISTMON              
         XC    LASTKEY,LASTKEY                                                  
         B     D05                                                              
*                                                                               
D60      DS    0H                                                               
         OI    GENSTAT2,USMYOK+STLCOK USE MY OK MESSAGE + LOWER CASE            
         MVC   CONHEAD(36),=C'LIST DIPLAYED - INPUT CHANGES OR HIT'             
         MVC   CONHEAD+36(15),=C' ENTER FOR NEXT'                               
*                                                                               
DX       B     EXIT                                                             
         DROP  R3,R4                                                            
         EJECT                                                                  
*                                                                               
*        FIND RULE TYPE NAME IN TABLE                                           
*                                                                               
FNDRULE  NTR1                                                                   
         LA    R1,RULETAB                                                       
         MVC   WORK,SPACES                                                      
*                                                                               
FR10     CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   BYTE,10(R1)                                                      
         BE    FR20                                                             
         LA    R1,L'RULETAB(R1)                                                 
         B     FR10                                                             
*                                                                               
FR20     MVC   WORK(10),0(R1)                                                   
         B     EXIT                                                             
         EJECT                                                                  
ADDNEW   NTR1                                                                   
         L     R4,AIO                                                           
         USING RULRECD,R4                                                       
         MVC   RULKTYP,=X'0D92'                                                 
         MVC   RULKAGMD,BAGYMD     AGY/MEDIA                                    
         MVC   RULKCLT,BCLT        CLIENT                                       
         OC    SVPGRP,SVPGRP       IS THERE A PRODUCT GROUP                     
         BZ    AN10                                                             
         MVC   RULKPGCD(3),SVPGRP  PRODUCT GROUP                                
         B     AN20                                                             
*                                                                               
AN10     MVC   RULKPRD,BPRD        PRODUCT                                      
*                                                                               
AN20     MVC   RULKEST,BEST        ESTIMATE                                     
         MVC   RULKDPT,SVDPT       DAYPART                                      
         MVC   RULKRUL,SVRULE      RULE                                         
         MVC   RULKEST,BEST        ESTIMATE                                     
         MVC   RULKRUL,BYTE        SET RULE TYPE IN RECORD                      
         MVC   RULAGYA,AGENCY                                                   
         MVI   RULLEN+1,24                                                      
         GOTO1 ADDREC                                                           
*                                                                               
         L     R4,AIO                                                           
         MVC   KEY,0(R4)                                                        
         GOTO1 HIGH                                                             
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*        ADD RULE DATA ELEMENTS                                                 
*                                                                               
ADDRULES NTR1                                                                   
         LA    R4,ELEMENT          ADD NEW ELEMENT                              
         XC    ELEMENT,ELEMENT                                                  
         USING RULDATAD,R4                                                      
         MVI   RULDCD,X'05'                                                     
         ZIC   R1,0(R5)                                                         
         STC   R1,RULDLEND         LENGTH OF DATA                               
         LA    R1,3(R1)                                                         
         STC   R1,RULDLN           SET LENGTH OF ELEMENT                        
         SH    R1,=H'4'            1 FOR EX & 3 FOR LEN                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   RULDDATA(0),12(R5)                                               
         GOTO1 ADDELEM                                                          
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* SUBROUTINE TO MOVE RECORD IN DUB TO RECORD IN DUB+4                           
*                                                                               
         SPACE 1                                                                
COPYREC  NTR1                                                                   
         L     RE,DUB                                                           
         L     RF,DUB+4                                                         
         LA    R0,8                *** ASSUME REC LEN = 2000 ***                
*                                                                               
         MVC   0(250,RF),0(RE)                                                  
         LA    RE,250(RE)                                                       
         LA    RF,250(RF)                                                       
         BCT   R0,*-14                                                          
         B     EXIT                                                             
         SPACE 2                                                                
*                                                                               
* SUBROUTINE TO COMPARE RECORD IN DUB TO RECORD IN DUB+4                        
*                                                                               
         SPACE 1                                                                
COMPREC  NTR1                                                                   
         L     R4,DUB                                                           
         L     R3,DUB+4                                                         
*                                                                               
         CLC   0(24,R4),0(R3)      COMPARE KEYS                                 
         BNE   NO                                                               
         LA    R4,24(R4)           BUMP TO FIRST ELEMENT                        
         LA    R3,24(R3)                                                        
*                                                                               
CR10     CLI   0(R4),X'F1'         IF WE REACHED ACTIVITY ELEMENT               
         BNE   CR20                                                             
         CLI   0(R3),X'F1'         ON BOTH RECORD - DONE                        
         BE    CRX                                                              
*                                                                               
CR20     ZIC   R1,1(R4)            GET L'ELEMENT IN AIO1                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R4),0(R3)       IF ELEMENTS DON'T MATCH - NEQ                
         BNE   NO                                                               
         LA    R1,1(R1)                                                         
         AR    R4,R1               BUMP TO NEXT ELEMENT                         
         AR    R3,R1                                                            
         B     CR10                                                             
*                                                                               
CRX      B     YES                                                              
         EJECT                                                                  
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
TRAPERR  OC    ERRDISP,ERRDISP     DO I NEED TO OVERRIDE CURSOR POS.            
         BZ    TRAPEND                                                          
         L     RE,SYSPARMS                                                      
         L     RE,0(RE)            RE=A(TRANSLATOR I/O BLOCK)                   
         USING TIOBD,RE                                                         
         OI    TIOBINDS,TIOBSETC   OVERRIDE CURSOR POSITION                     
         LR    RF,R2                                                            
         SR    RF,RA                                                            
         STCM  RF,3,TIOBCURD       DISPLACEMENT TO FIELD HEADER                 
         MVC   TIOBCURI,ERRDISP+1  DISPLACMENT INTO FIELD                       
*                                                                               
TRAPEND  GOTO1 ERREX               NEVER TO RETURN                              
         SPACE 2                                                                
         GETEL (R4),DATADISP,ELCODE                                             
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
MYSPECS  DS    0H                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,60,REQUESTOR                                                  
         SSPEC H2,60,REPORT                                                     
         SSPEC H2,73,PAGE                                                       
         SPACE 1                                                                
         SSPEC H1,32,C'BUYING RULES LIST'                                       
         SSPEC H2,32,C'-----------------'                                       
         SPACE 1                                                                
         SSPEC H4,1,C'TYPE'                                                     
         SSPEC H4,12,C'------------------------- D A T A ------------'          
         SSPEC H4,58,C'------------'                                            
         SPACE 1                                                                
         DC    X'00'                                                            
         SPACE 2                                                                
         DS    0D                                                               
RULETAB  DS    0CL11                                                            
         DC    CL10'DAYPART   ',X'05'                                           
         DC    CL10'HOLIDAY   ',X'10'                                           
         DC    CL10'PROGRAM   ',X'20'                                           
         DC    CL10'RATING    ',X'25'                                           
         DC    CL10'SPOTS/DAY ',X'30'                                           
         DC    CL10'SPOTS/WEEK',X'32'                                           
         DC    CL10'STATION   ',X'40'                                           
         DC    CL10'TIME      ',X'50'                                           
         DC    X'FF'                                                            
         EJECT                                                                  
       ++INCLUDE SPSFMFFD                                                       
         EJECT                                                                  
         PRINT   OFF                                                            
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         PRINT   ON                                                             
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFM91D          DSECT FOR RECORD LISTING.                    
         EJECT                                                                  
         ORG   SRUWORK                                                          
ERRDISP  DS    H                                                                
SVDPT    DS    CL1                                                              
SVPGRP   DS    CL3                                                              
DRYRUN   DS    CL1                                                              
LINECNT  DS    XL1                                                              
KEYCHG   DS    CL1                                                              
REDISP   DS    CL1                                                              
PREVRUL  DS    CL1                                                              
SVRULNM  DS    CL10                                                             
SVRULE   DS    CL10                                                             
LASTKEY  DS    CL(L'KEY)                                                        
FIRSTKEY DS    CL(L'KEY)                                                        
MYSVKEY  DS    CL(L'KEY)                                                        
COUNTER  DS    PL6                                                              
       ++INCLUDE SPSFMWORKD                                                     
         SPACE 2                                                                
LINED    DSECT                                                                  
LTYPEH   DS    CL8                                                              
LTYPE    DS    CL10                                                             
LDATAH   DS    CL8                                                              
LDATA    DS    CL60                                                             
LINNEXT  EQU   *-LINED                                                          
LLNQ     EQU   *-LINED                                                          
         EJECT                                                                  
       ++INCLUDE SPGENRULE                                                      
         EJECT                                                                  
       ++INCLUDE SPGENPRG                                                       
         EJECT                                                                  
*DDSPLWORKD                                                                     
*DDSPOOLD                                                                       
*FATIOB                                                                         
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE FATIOB                                                         
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'067SPSFM41   05/01/02'                                      
         END                                                                    
