*          DATA SET ACPRO39    AT LEVEL 042 AS OF 04/11/07                      
*PHASE T60B39A                                                                  
         TITLE 'T60B39 - PRODUCTION ESTIMATE EDIT MODULE'                       
T60B39   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T60B39**,RA,RR=R2                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          SYSTEM SPECIFIC WORK                         
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
         SPACE 1                                                                
         ST    R2,RELO                                                          
*                                                                               
EST1     CLI   MODE,VALREC                                                      
         BNE   ESTX                                                             
*                                                                               
         LA    RE,LOCAL                                                         
         LA    RF,LOCALLN                                                       
         SR    R1,R1                                                            
         MVCL  RE,R0               CLEAR LOCAL WORKING STORAGE                  
*                                                                               
EST2     CLI   OFFLINE,C'Y'        TEST OFFLINE                                 
         BNE   EST3                                                             
         L     RE,TWADCONS                                                      
         USING TWADCOND,RE                                                      
         L     R0,TSPFUSER                                                      
         LA    R1,SVDATAL                                                       
         LA    RE,SVDATA                                                        
         LR    RF,R1                                                            
         MVCL  RE,R0               RESTORE SAVED VALUES                         
*                                                                               
EST3     BAS   RE,VALHED                                                        
         CLI   QREQUEST,C'N'       TEST NO REQUEST DETAILS                      
         BNE   *+8                                                              
         OI    GENSTAT2,NOREQDET   TELL GENCON TO SKIP DETAILS                  
*                                                                               
         CLI   QMAXLINE,0          TEST MAXLINES OVERRIDE                       
         BE    ESTX                NO                                           
*                                                                               
         ICM   RE,15,ABOX                                                       
         BZ    ESTX                NO BOX BLOCK AVAILABLE                       
         USING BOXD,RE                                                          
         ZIC   R1,QMAXLINE                                                      
         BCTR  R1,0                                                             
         STC   R1,BOXMAXL          SET OVERRIDE FOR MAXLINES                    
         B     ESTX                MAKES REQUEST DETAILS RIGHT                  
         DROP  RE                                                               
*                                                                               
ESTX     XMOD1 1                                                                
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
* SUB-ROUTINE TO VALIDATE HEADLINE FIELDS                                       
*                                                                               
VALHED   NTR1                                                                   
         LA    R2,CONRECH                                                       
         MVI   OPTION,C'Y'         DISPLAY NAMES FROM OGROUP TO JOB             
         GOTO1 SETHEIR                                                          
*                                                                               
* VALIDATE REPORT TYPE                                                          
*                                                                               
         LA    R2,ESTTYPEH         REPORT TYPE                                  
         GOTO1 ANY                                                              
         MVC   QTYPE,WORK                                                       
         MVI   ERROR,INVALID                                                    
         CLI   QTYPE,C'L'          TEST 'L'=LIVE                                
         BE    VALHED1                                                          
         CLI   QTYPE,C'D'          TEST 'D'=DRAFT                               
         BNE   ERREND                                                           
*                                                                               
         MVC   RCPROG+2(2),=C'ED'  FUDGE THE REPORT CODE                        
         MVC   QCRDCODE,=C'EY'     AND EOD HANDLE FOR DRAFT                     
*                                                                               
* VALIDATE OFFICE GROUP                                                         
*                                                                               
VALHED1  LA    R2,ESTOGH          OFFICE GROUP                                  
         CLI   5(R2),0                                                          
         BE    VALHED2                                                          
         GOTO1 VALOG                                                            
         MVC   QOG,EFFOFG                                                       
         SPACE 1                                                                
* VALIDATE OFFICE                                                               
*                                                                               
VALHED2  LA    R2,ESTOFFH          OFFICE                                       
         CLI   5(R2),0                                                          
         BE    VALHED4                                                          
         MVI   ERROR,NOTOFNOG      NOT COMPATIBLE WITH OFFICE GROUP             
         CLI   ESTOGH+5,0                                                       
         BNE   ERREND                                                           
         GOTO1 VALOFF                                                           
         MVC   QOFF,EFFOFFC                                                     
         SPACE 1                                                                
* VALIDATE CLIENT                                                               
*                                                                               
VALHED4  LA    R2,ESTCLIH          CLIENT                                       
         CLI   5(R2),0                                                          
         BE    VALHED6                                                          
         MVI   ERROR,NOTCLNOG      NOT COMPATIBLE WITH OFFICE GROUP             
         CLI   ESTOGH+5,0                                                       
         BNE   ERREND                                                           
         MVI   ERROR,NOTCLNOF      NOT COMPATIBLE WITH OFFICE                   
         CLI   ESTOFFH+5,0                                                      
         BNE   ERREND                                                           
         GOTO1 VALCLI                                                           
         MVC   QCLI,CLICODE                                                     
         SPACE 1                                                                
* VALIDATE PRODUCT                                                              
*                                                                               
VALHED6  LA    R2,ESTPROH          PRODUCT                                      
         CLI   5(R2),0                                                          
         BE    VALHED7                                                          
         MVI   ERROR,NEEDCLI       IF INPUT, NEED CLIENT AS WELL                
         CLI   ESTCLIH+5,0                                                      
         BE    ERREND                                                           
         GOTO1 VALPROD                                                          
         MVC   QPROD,PRODCODE                                                   
         B     VALHED8                                                          
         SPACE 1                                                                
VALHED7  LA    R2,ESTOGH           DID NOT INPUT PRODUCT                        
         MVI   ERROR,SOONERR                                                    
         TM    WHEN,X'20'          TEST FOR SOON REQUEST                        
         BO    ERREND              YES-EXIT WITH ERROR                          
         SPACE 1                                                                
* VALIDATE JOB                                                                  
*                                                                               
VALHED8  LA    R2,ESTJOBH          JOB                                          
         CLI   5(R2),0                                                          
         BE    VALHED10                                                         
         MVI   ERROR,NEEDPRO       IF INPUT, NEED PRODUCT AS WELL               
         CLI   ESTPROH+5,0                                                      
         BE    ERREND                                                           
         GOTO1 VALJOB                                                           
         MVC   QJOB,JOBNUM                                                      
         MVI   ERROR,BOESTERR                                                   
         TM    JOBJSTAT,XJOBSMCSE  JOB USES MCS ESTIMATES                       
         BO    ERREND                                                           
         MVI   ERROR,OLDESERR                                                   
         TM    JOBJSTAT,ACJBNEWQ   INSURE JOB USES NEW ESTIMATES                
         BZ    ERREND                                                           
         SPACE 1                                                                
* VALIDATE MEDIA GROUP                                                          
*                                                                               
VALHED10 MVI   OPTION,0            NO MORE NAME DISPLAYS                        
         LA    R2,ESTMGRH          MEDIA GROUP                                  
         CLI   5(R2),0                                                          
         BE    VALHED12                                                         
         MVI   ERROR,NOTJBNME      NOT COMPATIBLE WITH JOB                      
         CLI   ESTJOBH+5,0                                                      
         BNE   ERREND                                                           
         GOTO1 VALMG                                                            
         MVC   QMG,MGROUP                                                       
         SPACE 1                                                                
* VALIDATE MEDIA                                                                
*                                                                               
VALHED12 LA    R2,ESTMEDH          MEDIA                                        
         CLI   5(R2),0                                                          
         BE    VALHED14                                                         
         MVI   ERROR,NOTMENMG      NOT COMPATIBLE WITH MEDIA GROUP              
         CLI   ESTMGRH+5,0                                                      
         BNE   ERREND                                                           
         MVI   ERROR,NOTJBNME      NOT COMPATIBLE WITH JOB                      
         CLI   ESTJOBH+5,0                                                      
         BNE   ERREND                                                           
         GOTO1 VALMED                                                           
         MVC   QMED,MEDIA                                                       
         SPACE 1                                                                
* VALIDATE BILLING GROUP                                                        
*                                                                               
VALHED14 LA    R2,ESTBGRH                                                       
         CLI   5(R2),0                                                          
         BE    VALHED16                                                         
         GOTO1 ANY                                                              
         MVC   QBGR,WORK           SAVE BILLING GROUP                           
         CLI   WORK,C'*'           TEST FOR NEGATIVE FILTER                     
         BNE   VALHED16                                                         
         MVI   ERROR,INVALID                                                    
         CLI   5(R2),2                                                          
         BL    ERREND                                                           
         MVC   QBGR,WORK+1                                                      
         NI    QBGR,X'FF'-X'40'    SET NEGATIVE BIT ON                          
*                                                                               
* VALIDATE BILLING TYPE                                                         
*                                                                               
VALHED16 LA    R2,ESTBTYPH                                                      
         CLI   5(R2),0                                                          
         BE    VALHED18                                                         
*                                                                               
         CLI   5(R2),1             HANDLE THE SPECIALS                          
         BNE   VALHED17                                                         
         CLI   8(R2),C'E'          TEST ALL % OF ESTIMATE TYPES                 
         BE    *+12                YES                                          
         CLI   8(R2),C'S'          TEST ALL SPECIAL AMOUNT TYPES                
         BNE   VALHED17                                                         
         MVC   QBT,8(R2)                                                        
         B     VALHED18                                                         
*                                                                               
VALHED17 MVI   ERROR,INVALID                                                    
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM             BUILD SKELTON OPTION ELEMENT                 
         USING ACOPD,R6                                                         
         MVI   ACOPEL,ACOPELQ                                                   
         MVI   ACOPNUM,OPNBT       BILLING TYPE OPTION                          
         GOTO1 VVALOPT,DMCB,(R2),(R6)                                           
         BNE   ERREND                                                           
         ZIC   R1,ACOPLEN                                                       
         SH    R1,=Y(ACOPDATA-ACOPD+1)                                          
         EX    R1,*+8                                                           
         B     VALHED18                                                         
         MVC   QBT(0),ACOPDATA     EXTRACT OPTION DATA                          
* --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---           
*                                                                               
* VALIDATE FILTER 1                                                             
*                                                                               
VALHED18 LA    R2,ESTFIL1H                                                      
         CLI   5(R2),0                                                          
         BE    VALHED19                                                         
         GOTO1 VALFIL,PARAS,QFILT,L'QFILT                                       
*                                                                               
* VALIDATE FILTER 2                                                             
*                                                                               
VALHED19 LA    R2,ESTFIL2H                                                      
         CLI   5(R2),0                                                          
         BE    VALHED20                                                         
         GOTO1 VALFIL,PARAS,QFILT2,L'QFILT2                                     
*                                                                               
* VALIDATE FILTER 3                                                             
*                                                                               
VALHED20 LA    R2,ESTFIL3H                                                      
         CLI   5(R2),0                                                          
         BE    VALHED21                                                         
         GOTO1 VALFIL,PARAS,QFILT3,L'QFILT3                                     
*                                                                               
* VALIDATE FILTER 4                                                             
*                                                                               
VALHED21 LA    R2,ESTFIL4H                                                      
         CLI   5(R2),0                                                          
         BE    VALHED22                                                         
         GOTO1 VALFIL,PARAS,QFILT4,L'QFILT4                                     
*                                                                               
* VALIDATE FILTER 5                                                             
*                                                                               
VALHED22 LA    R2,ESTFIL5H                                                      
         CLI   5(R2),0                                                          
         BE    VALHED24                                                         
         GOTO1 VALFIL,PARAS,QFILT5,L'QFILT5                                     
* --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---           
*                                                                               
* START DATE                                                                    
*                                                                               
VALHED24 LA    R2,ESTSTH           START DATE                                   
         CLI   5(R2),0                                                          
         BE    VALHED26                                                         
         GOTO1 VALIDATE,PARAS,DUB                                               
         GOTO1 DATCON,DMCB,DUB,(1,QSTART)                                       
*                                                                               
* END DATE                                                                      
*                                                                               
VALHED26 MVC   QEND,=3X'FF'                                                     
         LA    R2,ESTENDH                                                       
         CLI   5(R2),0                                                          
         BE    VALHED28                                                         
         GOTO1 VALIDATE,PARAS,DUB                                               
         GOTO1 DATCON,DMCB,DUB,(1,QEND)                                         
*                                                                               
* CLOSED JOBS                                                                   
*                                                                               
VALHED28 MVI   QCLOSE,C'N'         NO CLOSED JOBS                               
         LA    R2,ESTCLOSH                                                      
         CLI   5(R2),0                                                          
         BE    VALHED30                                                         
         MVC   QCLOSE,8(R2)                                                     
         CLI   8(R2),C'Y'                                                       
         BE    VALHED30                                                         
         CLI   8(R2),C'C'          TEST 'C'=CLOSED ONLY                         
         BE    VALHED30                                                         
         CLI   8(R2),C'N'                                                       
         BNE   ERREND                                                           
*                                                                               
* LOCKED JOBS                                                                   
*                                                                               
VALHED30 MVI   QLOCK,C'Y'          DEFAULT=INCLUDE LOCKED JOBS                  
         LA    R2,ESTLOCKH                                                      
         CLI   5(R2),0                                                          
         BE    VALHED32                                                         
         MVC   QLOCK,8(R2)                                                      
         CLI   8(R2),C'Y'                                                       
         BE    VALHED32                                                         
         CLI   8(R2),C'L'          TEST 'L'=LOCKED ONLY                         
         BE    VALHED32                                                         
         CLI   8(R2),C'N'                                                       
         BNE   ERREND                                                           
*                                                                               
* VALIDATE OPTIONS FIELDS                                                       
*                                                                               
VALHED32 LA    R2,ESTOPT1H                                                      
         CLI   5(R2),0             TEST FOR INPUT                               
         BNE   *+16                                                             
         CLI   ESTOPT2H+5,0        TEST FOR INPUT IN SECOND FIELD               
         BE    VALHED34                                                         
         B     ERREND                                                           
*                                                                               
         BAS   RE,VALOPT                                                        
*                                                                               
* VALIDATE COLUMN FIELDS                                                        
*                                                                               
VALHED34 SR    R4,R4               R4=COLUMN NUMBER                             
         LA    R2,ESTCOL1H         R2=A(FIELD HEADER)                           
         LA    R3,MAXCOLS          R3=LOOP COUNTER                              
         CLI   5(R2),0             LOOK FOR AT LEAST ONE COLUMN                 
         BNE   VALHED35                                                         
         BAS   RE,BUMPTOUN                                                      
         BCT   R3,*-12                                                          
         MVI   ERROR,MISSING                                                    
         LA    R2,ESTCOL1H                                                      
         B     ERREND                                                           
*                                                                               
VALHED35 CLI   5(R2),0             TEST FOR INPUT                               
         BE    VALHED36            NO                                           
         LA    R4,1(R4)            INCREMENT COLUMN NUMBER                      
         GOTO1 VALCOL,(R4)                                                      
*                                                                               
VALHED36 BAS   RE,BUMPTOUN                                                      
         BCT   R3,VALHED35                                                      
         STC   R4,NCOLS            SAVE N'COLUMNS                               
         CLI   QOVER,C'Y'          TEST OVERESTIMATE OPTION                     
         BNE   *+8                                                              
         BAS   RE,ADDXTRA          YES-ADD EXTRA COLUMNS                        
*                                                                               
* VALIDATE DETAIL FIELDS                                                        
*                                                                               
VALHED38 LA    R2,ESTDET1H                                                      
         CLI   5(R2),0             TEST FOR INPUT                               
         BNE   *+16                                                             
         CLI   ESTDET2H+5,0        TEST FOR INPUT IN SECOND FIELD               
         BE    VALHED40                                                         
         B     ERREND                                                           
*                                                                               
         BAS   RE,VALDET           VALIDATE DETAIL INPUT                        
*                                                                               
VALHED40 DS    0H                                                               
         SPACE 1                                                                
VALHEDX  B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO VALIDATE THE FILTER FIELD                                      
* AT ENTRY, R2=A(FLDH), P1=A(OUTPUT), P2=N'OUTPUT POSITIONS                     
*                                                                               
VALFIL   NTR1  ,                                                                
         L     R3,0(R1)            R3=A(OUTPUT)                                 
         L     R5,4(R1)            R5=N'OUTPUT POSITIONS                        
         MVI   ERROR,INVALID                                                    
         LA    R4,8(R2)            R4=A(INPUT)                                  
         ZIC   R6,5(R2)                                                         
         LR    R1,R5                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     VALFIL2                                                          
         MVC   0(0,R3),SPACES      PRE-CLEAR OUTPUT TO SPACES                   
*                                                                               
VALFIL2  CLI   0(R4),C'*'          TEST FOR NEGATIVE FILTER                     
         BE    VALFIL4             YES                                          
         CLI   0(R4),C' '          TEST ANY FILTER IN POSITION                  
         BE    VALFIL6             YES                                          
         CLI   0(R4),C'.'          TEST FOR NO FILTER VALUE                     
         BE    VALFIL3                                                          
         LA    R0,1                                                             
         GOTO1 TSTAN,(R4)                                                       
*                                                                               
VALFIL3  MVC   0(1,R3),0(R4)       SET VALUE                                    
         B     VALFIL6                                                          
*                                                                               
VALFIL4  LA    R4,1(R4)            NEXT INPUT CHARACTER                         
         SH    R6,=H'1'                                                         
         BZ    ERREND                                                           
         LA    R0,1                                                             
         GOTO1 TSTAN,(R4)                                                       
         MVC   0(1,R3),0(R4)                                                    
         NI    0(R3),X'FF'-X'40'   TURN OFF UPPER CASE BIT                      
*                                                                               
VALFIL6  LA    R3,1(R3)                                                         
         LA    R4,1(R4)                                                         
         SH    R6,=H'1'                                                         
         BZ    VALFILX             NO MORE INPUT TO EDIT                        
         BCT   R5,VALFIL2                                                       
         B     ERREND              EXCEEDED FILTER LIMIT                        
*                                                                               
VALFILX  B      XIT                                                             
         EJECT                                                                  
* SUB-ROUTINE TO CHECK FOR ALPHANUMERIC FIELD                                   
*                                                                               
* AT ENTRY, R0=N'BYTES TO CHECK, R1=A(STRING TO CHECK)                          
*                                                                               
TSTAN    ST    RE,SAVERE                                                        
         MVI   ERROR,INVALID                                                    
*                                                                               
TSTAN1   CLI   0(R1),C'0'                                                       
         BL    *+16                                                             
         CLI   0(R1),C'9'                                                       
         BH    ERREND                                                           
         B     TSTAN2                                                           
         CLI   0(R1),C'A'                                                       
         BL    ERREND                                                           
         CLI   0(R1),C'I'                                                       
         BNH   TSTAN2              CHARACTER IS BETWEEN A-I                     
         CLI   0(R1),C'J'                                                       
         BL    ERREND                                                           
         CLI   0(R1),C'R'                                                       
         BNH   TSTAN2                                                           
         CLI   0(R1),C'S'                                                       
         BL    ERREND                                                           
         CLI   0(R1),C'Z'                                                       
         BH    ERREND                                                           
*                                                                               
TSTAN2   LA    R1,1(R1)            NEXT CHARACTER IN FIELD                      
         BCT   R0,TSTAN1                                                        
*                                                                               
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
******************************************************************              
* SUB-ROUTINE TO VALIDATE THE OPTIONS FIELDS--CALLED FROM VALHED *              
******************************************************************              
         SPACE 1                                                                
VALOPT   NTR1  ,                                                                
         L     R4,AIO3             BUILD SCAN STRING AT IO3                     
         XC    0(8,R4),0(R4)                                                    
         LA    R2,ESTOPT1H         FIRST OPTIONS FIELD                          
         MVC   5(1,R4),5(R2)       EXTRACT DATA LENGTH                          
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R4),8(R2)                                                    
         LA    R1,9(R1)            COMPUTE TOTAL LENGTH                         
         STC   R1,0(R4)                                                         
         LA    RE,0(R1,R4)         POINT TO END OF STRING                       
*                                                                               
VALOPT2  LA    R2,ESTOPT2H                                                      
         CLI   5(R2),0             TEST FOR ANYTHING IN SECOND FIELD            
         BE    VALOPT4             NO                                           
*                                                                               
         ZIC   R1,5(R2)                                                         
         ZIC   RF,5(R4)                                                         
         LA    RF,0(R1,RF)         COMPUTE DATA LENGTH                          
         STC   RF,5(R4)                                                         
         LA    RF,8(RF)                                                         
         STC   RF,0(R4)                                                         
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),8(R2)       ATTACH REST OF STRING                        
*                                                                               
VALOPT4  XC    BLOCK(240),BLOCK                                                 
         XC    BLOCK+240(240),BLOCK+240                                         
         GOTO1 PARSNIP,DMCB,AIO3,BLOCK,('PSNNONLQ',0)                           
         LA    R2,ESTOPT1H                                                      
         MVI   ERROR,INVALID                                                    
         CLI   8(R1),0             TEST FOR PARSNIP ERROR                       
         BE    VALOPT6             NO                                           
*                                                                               
         SR    R3,R3                                                            
         ICM   R3,7,9(R1)                                                       
         B     VALOPTR                                                          
*                                                                               
VALOPT6  ZIC   R2,4(R1)            R2=N'COMPONENTS                              
         LA    R4,BLOCK                                                         
         USING PSND,R4             R4=A(PARSNIP BLOCK)                          
*                                                                               
VALOPT10 L     R3,PSNCOMP          R3=A(COMPONENT)                              
         CLI   PSNTAG,PSNFLDQ      TEST FOR KEYWORD FIELD                       
         BNE   VALOPTR                                                          
*                                                                               
         LA    R5,OPTTAB                                                        
         USING OPTTABD,R5                                                       
         LA    R0,OPTIONS          R0=LOOP COUNTER                              
*                                                                               
         MVI   ERROR,INVALID                                                    
         CLI   PSNLEN,L'OPTNAME    TEST LONGER THAN KEYWORD                     
         BH    VALOPTR                                                          
         ZIC   R1,PSNLEN                                                        
         BCTR  R1,0                                                             
*                                                                               
VALOPT15 CLC   PSNLEN,OPTMINL      TEST FOR MINIMUM LENGTH                      
         BL    VALOPT17            NO                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R3),OPTNAME                                                  
         BE    VALOPT20                                                         
*                                                                               
VALOPT17 LA    R5,OPTTABL(R5)                                                   
         BCT   R0,VALOPT15                                                      
         B     VALOPTR                                                          
*                                                                               
VALOPT20 MVI   ERROR,DUPINPUT      DUPLICATE INPUT CHECK                        
         MVC   HALF,OPTDISP                                                     
         LH    RE,HALF             DISP TO STORAGE VALUE                        
         LA    RE,SUBSYSD(RE)      POINT TO VALUE                               
         ZIC   R1,OPTLEN                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         OC    0(0,RE),0(RE)       TEST IF ANYTHING THERE                       
         BNZ   VALOPTR                                                          
*                                                                               
         TM    OPTIND,OPTIYES      TEST TO SET C'Y' IN FIELD                    
         BZ    *+12                                                             
         MVI   0(RE),C'Y'                                                       
         B     VALOPT30                                                         
*                                                                               
         TM    OPTIND,OPTIVAL      TEST FOR REQUIRED VALUE                      
         BZ    VALOPT25                                                         
*                                                                               
         MVI   ERROR,MISSING                                                    
         ZIC   R1,PSNLEN                                                        
         LA    R3,1(R1,R3)                                                      
         BCT   R2,*+8                                                           
         B     VALOPTR             NO MORE PARSNIP BLOCKS                       
         LA    R4,PSNL(R4)                                                      
         MVI   ERROR,INVALID                                                    
         L     R3,PSNCOMP                                                       
         CLI   PSNTAG,PSNVALQ                                                   
         BNE   VALOPTR                                                          
*                                                                               
VALOPT25 SR    RF,RF                                                            
         ICM   RF,3,OPTROUT        DISP TO ROUTINE                              
         LA    RF,T60B39(RF)                                                    
         BASR  RE,RF                                                            
*                                                                               
VALOPT30 LA    R4,PSNL(R4)         NEXT PARSNIP ENTRY                           
         BCT   R2,VALOPT10                                                      
*                                                                               
VALOPTX  B     XIT                                                              
*                                                                               
* ROUTINE TO VALIDATE DOWNLOAD OPTION                                           
*                                                                               
VALDOWN  MVI   QDOWN,C'Y'                                                       
         OC    JOBNUM,JOBNUM                                                    
         BNZR  RE                                                               
         MVI   ERROR,SUPPLIED                                                   
         MVC   CONHEAD(L'DOWNMSG),DOWNMSG                                       
         B     VALOPTR                                                          
*                                                                               
* ROUTINE TO VALIDATE SCHEME OPTION                                             
*                                                                               
VALSC    NTR1  ,                                                                
         MVI   ERROR,INP2LONG                                                   
         CLI   PSNLEN,8                                                         
         BH    VALOPTR                                                          
         ZIC   R1,PSNLEN                                                        
         BCTR  R1,0                                                             
         MVC   QSCHEME,SPACES                                                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   QSCHEME(0),0(R3)                                                 
*                                                                               
VALSC2   LA    R5,KEY                                                           
         USING ACSHKEY,R5                                                       
         XC    ACSHKEY,ACSHKEY                                                  
         MVI   ACSHRTYP,ACSHEQU                                                 
         MVI   ACSHSREC,ACSHSEQU                                                
         MVC   ACSHCUL,CUL                                                      
         MVC   ACSHCODE,QSCHEME                                                 
         GOTO1 HIGH                                                             
         CLC   ACSHKEY,KEYSAVE                                                  
         BE    VALSCX                                                           
         MVI   ERROR,BADSCH                                                     
         B     VALOPTR                                                          
*                                                                               
VALSCX   B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
* ROUTINE TO VALIDATE MAXLINES OVERRIDE                                         
*                                                                               
VALMAXL  TM    PSNSTAT,PSNNUMQ     TEST FOR NUMERIC VALUE                       
         BO    *+12                YES                                          
         MVI   ERROR,NOTNUM                                                     
         B     VALOPTR                                                          
*                                                                               
         ICM   RF,15,PSNNUM        GET VALUE                                    
         CH    RF,=H'40'           TEST BETWEEN 40-112                          
         BL    VALOPTR                                                          
         CH    RF,=H'112'                                                       
         BH    VALOPTR                                                          
         STC   RF,QMAXLINE                                                      
         BR    RE                                                               
         SPACE 2                                                                
*                                                                               
* ERROR EXIT WITH CURSOR POSITIONING                                            
*                                                                               
VALOPTR  L     R1,AIO3             START OF STRING                              
         LA    R1,8(R1)            ADJUST FOR DUMMY FIELD HEADER                
         SR    R3,R1               DISPLACEMENT TO ERROR                        
         LA    R2,ESTOPT1H         R2=A(FIRST FIELD HEADER)                     
         CLM   R3,1,5(R2)          TEST ERROR IN FIRST FIELD                    
         BNH   VALOPTRX            YES                                          
         ZIC   R1,5(R2)                                                         
         LA    R1,1(R1)            ADD IN INSERTED SPACE                        
         SR    R3,R1               DISPLACEMENT INTO SECOND FIELD               
         LA    R2,ESTOPT2H                                                      
*                                                                               
VALOPTRX STC   R3,ERRNDX           SET INDEX                                    
         B     ERREND                                                           
         EJECT                                                                  
******************************************************************              
* SUB-ROUTINE TO VALIDATE THE DETAILS FIELDS--CALLED FROM VALHED *              
******************************************************************              
         SPACE 1                                                                
VALDET   NTR1  ,                                                                
         LA    R4,BLOCK            BUILD SCAN STRING IN BLOCK                   
         XC    0(8,R4),0(R4)                                                    
         LA    R2,ESTDET1H         FIRST OPTIONS FIELD                          
         MVC   5(1,R4),5(R2)       EXTRACT DATA LENGTH                          
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R4),8(R2)                                                    
         LA    R1,9(R1)            COMPUTE TOTAL LENGTH                         
         STC   R1,0(R4)                                                         
         LA    RE,0(R1,R4)         POINT TO END OF STRING                       
*                                                                               
VALDET2  LA    R2,ESTDET2H                                                      
         CLI   5(R2),0             TEST FOR ANYTHING IN SECOND FIELD            
         BE    VALDET4             NO                                           
*                                                                               
         ZIC   R1,5(R2)                                                         
         ZIC   RF,5(R4)                                                         
         LA    RF,0(R1,RF)         COMPUTE DATA LENGTH                          
         STC   RF,5(R4)                                                         
         LA    RF,8(RF)                                                         
         STC   RF,0(R4)                                                         
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),8(R2)       ATTACH REST OF STRING                        
*                                                                               
VALDET4  L     RE,AIO3             BUILD PARSNIP TABLE IN IO3                   
         LH    RF,=H'1000'         1000 BYTE OUGHT TO COVER IT                  
         XCEF                                                                   
         L     R3,AIO3                                                          
*                                                                               
         MVI   BYTE,PSNNONLQ+PSNMVOKQ                                           
         GOTO1 PARSNIP,DMCB,BLOCK,(R3),(BYTE,0)                                 
         LA    R2,ESTDET1H                                                      
         MVI   ERROR,INVALID                                                    
         CLI   8(R1),0             TEST PARSNIP ERROR                           
         BE    VALDET6                                                          
         SR    R3,R3                                                            
         ICM   R3,7,9(R1)                                                       
         B     VALDETR                                                          
*                                                                               
VALDET6  ZIC   R2,4(R1)            R2=N'COMPONENTS                              
         L     R4,AIO3                                                          
         USING PSND,R4             R4=A(PARSNIP BLOCK)                          
*                                                                               
VALDET10 L     R3,PSNCOMP          R3=A(COMPONENT)                              
         CLI   PSNTAG,PSNFLDQ      TEST FOR KEYWORD FIELD                       
         BNE   VALDETR                                                          
*                                                                               
         LA    R5,DETTAB                                                        
         USING DETTABD,R5                                                       
         LA    R0,DETAILS          R0=LOOP COUNTER                              
*                                                                               
         MVI   ERROR,INVALID                                                    
         CLI   PSNLEN,L'DETNAME    TEST LONGER THAN KEYWORD                     
         BH    VALDETR                                                          
         ZIC   R1,PSNLEN                                                        
         BCTR  R1,0                                                             
*                                                                               
VALDET15 CLC   PSNLEN,DETMINL      TEST FOR MINIMUM LENGTH                      
         BL    VALDET17            NO                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R3),DETNAME                                                  
         BE    VALDET20                                                         
*                                                                               
VALDET17 LA    R5,DETTABL(R5)                                                   
         BCT   R0,VALDET15                                                      
         B     VALDETR                                                          
*                                                                               
VALDET20 MVI   ERROR,DUPINPUT      DUPLICATE INPUT CHECK                        
         MVC   HALF,DETDISP                                                     
         LH    R6,HALF             DISP TO STORAGE VALUE                        
         LA    R6,SUBSYSD(R6)      POINT TO VALUE                               
         ZIC   R1,DETLEN                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         OC    0(0,R6),0(R6)       TEST IF ANYTHING THERE                       
         BNZ   VALDETR                                                          
*                                                                               
VALDET22 MVI   ERROR,MISSING                                                    
         ZIC   R1,PSNLEN                                                        
         LA    R3,1(R1,R3)         POINT PAST 'KEY='                            
         BAS   RE,NEXTFLD          NEXT PARSNIP                                 
         MVI   ERROR,INVALID                                                    
         L     R3,PSNCOMP                                                       
         CLI   PSNTAG,PSNVALQ      TEST FOR VALUE FIELD                         
         BNE   VALDETR                                                          
*                                                                               
VALDET23 TM    DETIND,DETIYN       TEST TO VALIDATE Y/N                         
         BZ    VALDET25                                                         
*                                                                               
         CLI   PSNLEN,3                                                         
         BH    VALDETR                                                          
         MVC   0(1,R6),0(R3)       COPY IN 'Y' OR 'N'                           
         ZIC   R1,PSNLEN                                                        
         BCTR  R1,0                                                             
         EX    R1,YESCOMP                                                       
         BE    VALDET30            OK                                           
         EX    R1,NOCOMP                                                        
         BE    VALDET30                                                         
         B     VALDETR                                                          
*                                                                               
VALDET25 SR    RF,RF                                                            
         ICM   RF,3,DETROUT        DISP TO ROUTINE                              
         LA    RF,T60B39(RF)                                                    
         BASR  RE,RF                                                            
*                                                                               
VALDET30 LA    R4,PSNL(R4)         NEXT PARSNIP ENTRY                           
         BCT   R2,VALDET10                                                      
*                                                                               
VALDETX  B     XIT                                                              
*                                                                               
* VALIDATE SPACE=1-4                                                            
*                                                                               
VALSPA   ST    RE,SAVERE           SAVE RETURN POINT                            
         MVI   ERROR,NOTNUM                                                     
         TM    PSNSTAT,PSNNUMQ                                                  
         BZ    VALDETR             NOT NUMERIC                                  
         MVI   ERROR,INVALID                                                    
         CLI   PSNLEN,1                                                         
         BNE   VALDETR                                                          
         CLI   0(R3),C'1'          TEST '1'-'4'                                 
         BL    VALDETR                                                          
         CLI   0(R3),C'4'                                                       
         BH    VALDETR                                                          
         MVN   0(1,R6),0(R3)       EXTRACT NUMERIC BITS                         
         L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
* VALIDATE CAT=A(,B,C...) OR CAT=*A(,B,C...) ALSO DET(AIL)=                     
*                                                                               
VALCATG  ST    RE,SAVERE                                                        
         CLI   PSNTAG,PSNVALQ      TEST FOR FIRST VALUE                         
         BNE   VALDETR                                                          
         MVI   NEGSW,C'N'                                                       
         LR    R5,R6                                                            
         BCTR  R5,0                KEEP POINTER AT COUNTER                      
         CLI   0(R3),C'*'          TEST FOR NEGATIVE FILTER                     
         BNE   VALCATG2            NO                                           
*                                                                               
         MVI   NEGSW,C'Y'                                                       
         ZIC   R1,PSNLEN                                                        
         BCTR  R1,0                                                             
         STC   R1,PSNLEN           ADJUST BLOCK LENGTH                          
         LA    R3,1(R3)            ADVANCE COMPONENT POINTER                    
*                                                                               
VALCATG2 ZIC   RE,0(R5)            INCREMENT CATEGORY COUNT                     
         LA    RE,1(RE)                                                         
         STC   RE,0(R5)                                                         
         CLI   0(R5),MAXCATS       TEST OVER MAXIMUM NUMBER                     
         BH    VALDETR                                                          
         CLI   PSNLEN,2                                                         
         BH    VALDETR             NO MORE THAN 2 BYTES                         
         MVC   0(1,R6),0(R3)       FIRST BYTE                                   
         MVI   1(R6),C' '                                                       
         CLI   PSNLEN,1            TEST 1 BYTE CODE                             
         BE    *+10                                                             
         MVC   1(1,R6),1(R3)       NO-GET SECOND BYTE                           
         CLI   NEGSW,C'Y'          TEST NEGATIVE FILTER                         
         BNE   *+8                                                              
         NI    0(R6),X'FF'-X'40'   YES                                          
         LA    R6,2(R6)            ADVANCE OUTPUT POINTER                       
*                                                                               
VALCATG4 LA    RE,PSNL(R4)         LOOK AT NEXT FIELD                           
         CLI   0(RE),0             TEST FOR EOT                                 
         BE    VALCATGX            YES                                          
         CLI   PSNTAG-PSND(RE),PSNFLDQ                                          
         BNE   VALCATGX                                                         
         CLI   PSNVSEP-PSND(RE),C'=' TEST IF KEYWORD FIELD                      
         BE    VALCATGX            YES                                          
*                                                                               
         BAS   RE,NEXTFLD                                                       
         L     R3,PSNCOMP                                                       
         B     VALCATG2                                                         
*                                                                               
VALCATGX L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
* VALIDATE WC=AA(,BB,CC...) OR WC=*AA(,BB,CC...)                                
*                                                                               
VALWC    ST    RE,SAVERE                                                        
         CLI   QSW,0               DO WE HAVE START OPTION ?                    
         BH    VALDETR             YES, ERROR                                   
         CLI   PSNTAG,PSNVALQ      TEST FOR FIRST VALUE                         
         BNE   VALDETR                                                          
         MVI   NEGSW,C'N'                                                       
         CLI   0(R3),C'*'          TEST FOR NEGATIVE FILTER                     
         BNE   VALWC2              NO                                           
*                                                                               
         MVI   NEGSW,C'Y'                                                       
         ZIC   R1,PSNLEN                                                        
         BCTR  R1,0                                                             
         STC   R1,PSNLEN           ADJUST BLOCK LENGTH                          
         LA    R3,1(R3)            ADVANCE COMPONENT POINTER                    
*                                                                               
VALWC2   ZIC   RE,NWC              INCREMENT WORKCODE COUNT                     
         LA    RE,1(RE)                                                         
         STC   RE,NWC                                                           
         CLI   NWC,MAXWORK         TEST IF MORE THAN MAX                        
         BH    VALDETR                                                          
         CLI   PSNLEN,2                                                         
         BH    VALDETR             NO MORE THAN 2 BYTES                         
         MVC   0(1,R6),0(R3)       FIRST BYTE                                   
         MVI   1(R6),C' '                                                       
         CLI   PSNLEN,1            TEST 1 BYTE CODE                             
         BE    *+10                                                             
         MVC   1(1,R6),1(R3)       NO-GET SECOND BYTE                           
         CLI   NEGSW,C'Y'          TEST NEGATIVE FILTER                         
         BNE   *+8                                                              
         NI    0(R6),X'FF'-X'40'   YES                                          
         LA    R6,2(R6)            ADVANCE OUTPUT POINTER                       
*                                                                               
VALWC4   LA    RE,PSNL(R4)         LOOK AT NEXT FIELD                           
         CLI   0(RE),0             TEST FOR EOT                                 
         BE    VALWCX              YES                                          
         CLI   PSNTAG-PSND(RE),PSNFLDQ                                          
         BNE   VALWCX                                                           
         CLI   PSNVSEP-PSND(RE),C'=' TEST FOR KEYWORD                           
         BE    VALWCX              YES-CANNOT BE ANOTHER WORKCODE               
*                                                                               
         BAS   RE,NEXTFLD                                                       
         L     R3,PSNCOMP                                                       
         B     VALWC2                                                           
*                                                                               
VALWCX   L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
* VALIDATE SW=AA                                                                
*                                                                               
VALSW    ST    RE,SAVERE                                                        
         CLI   QWC,0               DO WE HAVE A SELECT OPTION ?                 
         BH    VALDETR             YES, ERROR                                   
         CLI   PSNTAG,PSNVALQ      TEST FOR FIRST VALUE                         
         BNE   VALDETR                                                          
         CLI   PSNLEN,2                                                         
         BH    VALDETR             NO MORE THAN 2 BYTES                         
         MVC   0(1,R6),0(R3)       FIRST BYTE                                   
         MVI   1(R6),C' '                                                       
         CLI   PSNLEN,1            TEST 1 BYTE CODE                             
         BE    *+10                                                             
         MVC   1(1,R6),1(R3)       NO-GET SECOND BYTE                           
         LA    R6,2(R6)            ADVANCE OUTPUT POINTER                       
*                                                                               
         LA    RE,PSNL(R4)         LOOK AT NEXT FIELD                           
         CLI   0(RE),0             TEST FOR EOT                                 
         BE    VALSWX              YES                                          
         CLI   PSNTAG-PSND(RE),PSNFLDQ                                          
         BNE   VALSWX                                                           
         CLI   PSNVSEP-PSND(RE),C'=' TEST FOR KEYWORD                           
         BE    VALSWX              YES, MUST BE ANOTHER FIELD                   
         BAS   RE,NEXTFLD                                                       
         L     R3,PSNCOMP                                                       
         B     VALDETR                                                          
*                                                                               
VALSWX   L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
VALZERO  ST    RE,SAVERE                                                        
         CLI   PSNLEN,8            TEST FOR BIG FIELD                           
         BH    VALDETR                                                          
         MVC   0(1,R6),0(R3)       COPY IN FIRST BYTE                           
         ZIC   R1,PSNLEN                                                        
         BCTR  R1,0                                                             
         EX    R1,SUPCOMP          TEST FOR S(UPPRESS)                          
         BE    VALZEROX                                                         
         EX    R1,DISPCOMP         TEST FOR D(ISPLAY)                           
         BE    VALZEROX                                                         
         CLI   PSNLEN,3            TEST MAX SIZE OF PARM                        
         BH    VALDETR                                                          
         EX    R1,AGYCOMP          TEST FOR A(GY)                               
         BNE   VALDETR                                                          
*                                                                               
VALZEROX L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
SUPCOMP  CLC   0(0,R3),=C'SUPPRESS'                                             
DISPCOMP CLC   0(0,R3),=C'DISPLAY'                                              
AGYCOMP  CLC   0(0,R3),=C'AGY'                                                  
         SPACE 2                                                                
VALZ     ST    RE,SAVERE                                                        
         CLI   PSNLEN,1                                                         
         BNE   VALZ2                                                            
         CLI   0(R3),C'0'          TEST FOR NUMERIC ZERO                        
         BNE   VALZ2                                                            
         MVI   0(R6),C'0'                                                       
         B     VALZX                                                            
*                                                                               
VALZ2    CLI   PSNLEN,5            TEST INPUT NO MORE THAN 5 BYTES              
         BH    VALDETR                                                          
         MVC   0(1,R6),0(R3)       EXTRACT FIRST BYTE                           
         ZIC   R1,PSNLEN                                                        
         BCTR  R1,0                                                             
         EX    R1,ZEROCOMP                                                      
         BE    VALZX                                                            
         EX    R1,BLANCOMP                                                      
         BNE   VALDETR                                                          
*                                                                               
VALZX    L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
ZEROCOMP CLC   0(0,R3),=C'ZERO'                                                 
BLANCOMP CLC   0(0,R3),=C'BLANK'                                                
         SPACE 2                                                                
VALRND   ST    RE,SAVERE                                                        
         CLI   PSNLEN,3                                                         
         BH    VALDETR                                                          
         MVC   0(1,R6),0(R3)       FIRST BYTE IS VALUE                          
         ZIC   R1,PSNLEN                                                        
         BCTR  R1,0                                                             
         EX    R1,YESCOMP          ROUND=Y(ES),N(O),P                           
         BE    VALRNDX                                                          
         EX    R1,NOCOMP                                                        
         BE    VALRNDX                                                          
         CLI   PSNLEN,1                                                         
         BNE   VALDETR                                                          
         CLI   0(R3),C'P'          TEST FOR PENNY OPTION                        
         BNE   VALDETR                                                          
*                                                                               
VALRNDX  L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
VALDFT   ST    RE,SAVERE                                                        
         CLI   PSNLEN,3                                                         
         BH    VALDETR                                                          
         MVC   0(1,R6),0(R3)       FIRST BYTE IS VALUE                          
         ZIC   R1,PSNLEN                                                        
         BCTR  R1,0                                                             
         EX    R1,YESCOMP          DRAFT=Y(ES),N(O),O                           
         BE    VALDFTX                                                          
         EX    R1,NOCOMP                                                        
         BE    VALDFTX                                                          
         CLI   PSNLEN,1                                                         
         BNE   VALDETR                                                          
         CLI   0(R3),C'O'          TEST FOR ONLY OPTION                         
         BNE   VALDETR                                                          
*                                                                               
VALDFTX  L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
VALBOX   ST    RE,SAVERE                                                        
         CLI   PSNLEN,8                                                         
         BH    VALDETR                                                          
         MVC   0(1,R6),0(R3)       FIRST BYTE IS VALUE                          
         ZIC   R1,PSNLEN                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R3),=C'COMPRESS' BOX=Y(ES),N(0),C(OMPRESS)                   
         BE    VALBOXX                                                          
         CLI   PSNLEN,3            NOW CANNOT BE MORE THAN 3                    
         BH    VALDETR                                                          
         EX    R1,YESCOMP          BOX=Y(ES),N(O),P                             
         BE    VALBOXX                                                          
         EX    R1,NOCOMP                                                        
         BNE   VALDETR                                                          
*                                                                               
VALBOXX  L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
* OPTION TO SQUEEZE WORKCODE DESCRIPTION COLUMN FOR YNR ONLY                    
* ASSUMES THAT NO WORKCODE TEXT WILL BE USED                                    
*                                                                               
VALSQ    ST    RE,SAVERE                                                        
         TM    PSNSTAT,PSNNUMQ                                                  
         BO    VALSQ2                                                           
*                                                                               
         CLI   PSNLEN,3            VALIDATE FOR Y(ES)                           
         BH    VALDETR                                                          
         ZIC   R1,PSNLEN                                                        
         BCTR  R1,0                                                             
         EX    R1,YESCOMP                                                       
         BNE   VALDETR                                                          
         MVI   0(R6),9             SET 9 AS THE DEFAULT VALUE                   
         B     VALSQX                                                           
*                                                                               
VALSQ2   ICM   R1,15,PSNNUM                                                     
         CH    R1,=H'9'            TEST BETWEEN 9 AND 20                        
         BL    VALDETR                                                          
         CH    R1,=H'20'                                                        
         BH    VALDETR                                                          
         STC   R1,0(R6)                                                         
*                                                                               
VALSQX   L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
* ERROR EXIT WITH CURSOR POSITIONING                                            
*                                                                               
VALDETR  LA    R1,BLOCK            START OF STRING                              
         LA    R1,8(R1)            ADJUST FOR DUMMY FIELD HEADER                
         SR    R3,R1               DISPLACEMENT TO ERROR                        
         LA    R2,ESTDET1H         R2=A(FIRST FIELD HEADER)                     
         CLM   R3,1,5(R2)          TEST ERROR IN FIRST FIELD                    
         BNH   VALDETRX            YES                                          
         ZIC   R1,5(R2)                                                         
         LA    R1,1(R1)            ADD IN INSERTED SPACE                        
         SR    R3,R1               DISPLACEMENT INTO SECOND FIELD               
         LA    R2,ESTDET2H                                                      
*                                                                               
VALDETRX STC   R3,ERRNDX           SET INDEX                                    
         B     ERREND                                                           
         SPACE 2                                                                
* SUB-ROUTINE TO ADVANCE TO NEXT PARSNIP FIELD                                  
*                                                                               
NEXTFLD  LA    R4,PSNL(R4)                                                      
         BCTR  R2,RE                                                            
         B     VALDETR                                                          
         SPACE 2                                                                
YESCOMP  CLC   0(0,R3),=C'YES'                                                  
NOCOMP   CLC   0(0,R3),=C'NO '                                                  
         EJECT                                                                  
******************************************************************              
* SUB-ROUTINE TO VALIDATE THE COLUMN FIELDS--CALLED FROM VALHED--*              
* AT ENTRY, R1=COLUMN NUMBER, R2=A(FIELD HEADER)                 *              
*                                                                *              
* SYNTAX IS COLUMN EXPRESSION(,NAME1(/NAME2)                     *              
* JOBCOL EDITS COLUMN EXPRESSION AND NAMES ARE 14 BYTES          *              
******************************************************************              
         SPACE 1                                                                
VALCOL   NTR1  ,                                                                
         STC   R1,THISCOL                                                       
         XC    BLOCK(240),BLOCK                                                 
         XC    BLOCK+240(240),BLOCK+240                                         
         GOTO1 PARSNIP,DMCB,(R2),BLOCK,('PSNNONLQ',SEPTAB)                      
         CLI   8(R1),0             TEST FOR PARSNIP ERROR                       
         BE    VALCOL5                                                          
*                                                                               
         SR    R3,R3                                                            
         ICM   R3,7,9(R1)                                                       
         LA    RE,8(R2)            RE=A(START OF COLUMN FIELD)                  
         SR    R3,RE               COMPUTE DISP INTO FIELD                      
         STC   R3,ERRNDX                                                        
         B     INVEND                                                           
*                                                                               
VALCOL5  LA    R4,BLOCK                                                         
         USING PSND,R4                                                          
*                                                                               
VALCOL10 L     R3,PSNCOMP                                                       
         CLI   PSNTAG,PSNFLDQ                                                   
         BNE   INVEND              MUST START WITH A FIELD                      
*                                                                               
         BAS   RE,BLDFLD           BUILD A DUMMY FIELD HEADER                   
         GOTO1 VJOBCOL,DMCB,(1,AIO2),WORK,ACOMFACS                              
         CLI   4(R1),0                                                          
         BNE   VALCOL15            NO ERROR                                     
*                                                                               
         MVI   ERROR,COLERR                                                     
         MVC   ERRNDX,5(R1)        DISP TO ERROR                                
         B     ERREND                                                           
*                                                                               
VALCOL15 ZIC   R5,THISCOL                                                       
         BCTR  R5,0                                                             
         MH    R5,=Y(JBCLENQ)                                                   
         LA    R5,COLLIST(R5)                                                   
         MVC   0(JBCLENQ,R5),WORK  SLOT IN COLUMN ENTRY                         
*                                                                               
VALCOL20 MVC   NAME1,SPACES                                                     
         MVC   NAME2,SPACES                                                     
         LA    R4,PSNL(R4)         NEXT PARSNIP ENTRY                           
         CLI   PSNTAG,0            TEST NO MORE INPUT                           
         BE    VALCOL25                                                         
         LA    RF,NAME1                                                         
         BAS   RE,MOVENAME                                                      
*                                                                               
         ICM   R4,15,PSNVAL        TEST IF VALUE PRESENT                        
         BZ    VALCOL25            NO                                           
         LA    RF,NAME2                                                         
         BAS   RE,MOVENAME                                                      
*                                                                               
VALCOL25 ZIC   RE,THISCOL                                                       
         BCTR  RE,0                                                             
         MH    RE,=Y(L'NAMELIST)                                                
         LA    RE,NAMELIST(RE)                                                  
         MVC   0(L'NAMELIST,RE),NAME1 SLOT IN NAME VALUES                       
*                                                                               
VALCOLX  B     XIT                                                              
         SPACE 2                                                                
BLDFLD   ST    RE,SAVERE                                                        
         L     RE,AIO2                                                          
         XC    0(60,RE),0(RE)                                                   
         ZIC   R1,PSNLEN                                                        
         STC   R1,5(RE)            SET DATA LENGTH                              
         LA    RF,8(R1)                                                         
         STC   RF,0(RE)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,RE),0(R3)       EXTRACT DATA                                 
         L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
MOVENAME L     R3,PSNCOMP                                                       
         ZIC   R1,PSNLEN                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),0(R3)                                                    
         BR    RE                                                               
         SPACE 2                                                                
SEPTAB   DC    AL1(2),C'=/'                                                     
         DC    AL1(1),C','                                                      
         DC    AL1(1),C','                                                      
         DC    AL1(1),C','                                                      
         DS    0H                                                               
         EJECT                                                                  
****************************************************************                
* SUB-ROUTINE TO ADD EXTRA COLUMNS FOR OVERESTIMATE PROCESSING *                
* --CALLED FROM VALHED--                                       *                
****************************************************************                
         SPACE 1                                                                
ADDXTRA  NTR1  ,                                                                
         GOTO1 VJOBCOL,DMCB,(2,XTRACOLS),BLOCK,ACOMFACS                         
         ZIC   R5,NCOLS                                                         
         LR    R1,R5                                                            
         LA    R1,1(R1)            SET MAXCE COLUMN                             
         STC   R1,MAXCECOL                                                      
         LA    R1,1(R1)                                                         
         STC   R1,ACTCOL           SET ACTUALS COLUMN                           
         MH    R5,=Y(JBCLENQ)                                                   
         LA    R5,COLLIST(R5)                                                   
         MVC   0(JBCLENQ*2,R5),BLOCK                                            
*                                                                               
ADDXTRAX B     XIT                                                              
         EJECT                                                                  
*              SUPPORTING SUBROUTINES                                           
         SPACE 3                                                                
BUMP     ZIC   RF,0(R2)            BUMP TO NEXT HEADER                          
         AR    R2,RF                                                            
         BR    RE                                                               
         SPACE 1                                                                
BUMPTOUN ZIC   RF,0(R2)            BUMP TO NEXT UNPROTECTED                     
         AR    R2,RF                                                            
         CLI   0(R2),0                                                          
         BER   RE                                                               
         TM    1(R2),X'20'                                                      
         BO    BUMPTOUN                                                         
         BR    RE                                                               
         SPACE 1                                                                
OKXIT    CR    RB,RB                                                            
         B     XIT                                                              
         SPACE 1                                                                
NOTOKXIT LTR   RB,RB                                                            
         B     XIT                                                              
         SPACE 1                                                                
GETELIO  L     R6,AIO                                                           
         GETEL (R6),DATADISP,ELCODE                                             
         SPACE 1                                                                
INVEND   MVI   ERROR,INVALID       INVALID EXIT                                 
         B     ERREND                                                           
         SPACE 1                                                                
MYEND    MVI   ERROR,X'FE'         USING MY OWN ERROR MSG                       
         SPACE 1                                                                
ERREND   GOTO1 VERRCUR                                                          
         EJECT                                                                  
*              LTORG FOR THIS PHASE                                             
         SPACE 3                                                                
RELO     DS    A                                                                
         SPACE 1                                                                
PATCH    DS    0H                                                               
         DC    XL32'00'                                                         
         SPACE 2                                                                
* CONSTANTS                                                                     
*                                                                               
XTRACOLS DC    AL1(9+8),4X'00',X'09',2X'00',CL9'ACT,MAXCE'                      
DOWNMSG  DC    C'**ERROR-YOU CAN ONLY DOWNLOAD A REQUEST FOR ONE JOB**'         
         SPACE 2                                                                
* OPTIONS TABLE (COVERED BY OPTTABD)                                            
*                                                                               
         DS    0F                                                               
OPTTAB   DS    0CL(OPTTABL)                                                     
*                                                                               
         DC    CL8'DOWN'                                                        
         DC    AL1(4,1),AL1(0,0)                                                
         DC    AL2(QDOWN-SUBSYSD,VALDOWN-T60B39)                                
         DC    XL4'00'                                                          
*                                                                               
         DC    CL8'WIDE'                                                        
         DC    AL1(4,1),AL1(OPTIYES,0)                                          
         DC    AL2(QWIDE-SUBSYSD,0)                                             
         DC    XL4'00'                                                          
*                                                                               
         DC    CL8'OVEREST'                                                     
         DC    AL1(4,1),AL1(OPTIYES,0)                                          
         DC    AL2(QOVER-SUBSYSD,0)                                             
         DC    XL4'00'                                                          
*                                                                               
         DC    CL8'MAXLINES'                                                    
         DC    AL1(4,L'QMAXLINE),AL1(OPTIVAL,0)                                 
         DC    AL2(QMAXLINE-SUBSYSD,VALMAXL-T60B39)                             
         DC    XL4'00'                                                          
*                                                                               
OPTIONS  EQU   (*-OPTTAB)/L'OPTTAB                                              
         SPACE 2                                                                
* TABLE OF DETAIL KEYWORDS (COVERED BY DETTABD)                                 
*                                                                               
         DS    0F                                                               
DETTAB   DS    0CL(DETTABL)                                                     
*                                                                               
         DC    CL8'CATEGORY'                                                    
         DC    AL1(3,MAXCATS*L'QCAT),AL1(0,0)                                   
         DC    AL2(QCAT-SUBSYSD,VALCATG-T60B39)                                 
         DC    XL4'00'                                                          
*                                                                               
         DC    CL8'TEXT'                                                        
         DC    AL1(2,1),AL1(DETIYN,0)                                           
         DC    AL2(QTEXT-SUBSYSD,0)                                             
         DC    XL4'00'                                                          
*                                                                               
         DC    CL8'SPACES'                                                      
         DC    AL1(5,L'QSPACE),AL1(0,0)                                         
         DC    AL2(QSPACE-SUBSYSD,VALSPA-T60B39)                                
         DC    XL4'00'                                                          
*                                                                               
         DC    CL8'ZERO'                                                        
         DC    AL1(2,1),AL1(0,0)                                                
         DC    AL2(QZERO-SUBSYSD,VALZERO-T60B39)                                
         DC    XL4'00'                                                          
*                                                                               
         DC    CL8'MINUS'                                                       
         DC    AL1(2,1),AL1(DETIYN,0)                                           
         DC    AL2(QMINUS-SUBSYSD,0)                                            
         DC    XL4'00'                                                          
*                                                                               
         DC    CL8'BOXES'                                                       
         DC    AL1(3,1),AL1(0,0)                                                
         DC    AL2(QBOXES-SUBSYSD,VALBOX-T60B39)                                
         DC    XL4'00'                                                          
*                                                                               
         DC    CL8'WORK'                                                        
         DC    AL1(2,1),AL1(DETIYN,0)                                           
         DC    AL2(QWCDET-SUBSYSD,0)                                            
         DC    XL4'00'                                                          
*                                                                               
         DC    CL8'0'                                                           
         DC    AL1(1,1),AL1(0,0)                                                
         DC    AL2(QZEROES-SUBSYSD,VALZ-T60B39)                                 
         DC    XL4'00'                                                          
*                                                                               
         DC    CL8'CAPS'                                                        
         DC    AL1(4,1),AL1(DETIYN,0)                                           
         DC    AL2(QCAPS-SUBSYSD,0)                                             
         DC    XL4'00'                                                          
*                                                                               
         DC    CL8'REQUEST'                                                     
         DC    AL1(3,1),AL1(DETIYN,0)                                           
         DC    AL2(QREQUEST-SUBSYSD,0)                                          
         DC    XL4'00'                                                          
*                                                                               
         DC    CL8'JT'                                                          
         DC    AL1(2,1),AL1(DETIYN,0)                                           
         DC    AL2(QJOBTOT-SUBSYSD,0)                                           
         DC    XL4'00'                                                          
*                                                                               
         DC    CL8'ROUND'                                                       
         DC    AL1(2,1),AL1(0,0)                                                
         DC    AL2(QROUND-SUBSYSD,VALRND-T60B39)                                
         DC    XL4'00'                                                          
*                                                                               
         DC    CL8'CN'                                                          
         DC    AL1(2,1),AL1(DETIYN,0)                                           
         DC    AL2(QCN-SUBSYSD,0)                                               
         DC    XL4'00'                                                          
*                                                                               
         DC    CL8'COMMAS'                                                      
         DC    AL1(3,1),AL1(DETIYN,0)                                           
         DC    AL2(QCOMMAS-SUBSYSD,0)                                           
         DC    XL4'00'                                                          
*                                                                               
         DC    CL8'PBWORK'                                                      
         DC    AL1(3,1),AL1(DETIYN,0)                                           
         DC    AL2(QPBWORK-SUBSYSD,0)                                           
         DC    XL4'00'                                                          
*                                                                               
         DC    CL8'DES'                                                         
         DC    AL1(3,1),AL1(DETIYN,0)                                           
         DC    AL2(QDES-SUBSYSD,0)                                              
         DC    XL4'00'                                                          
*                                                                               
         DC    CL8'SQUEEZE'                                                     
         DC    AL1(2,1),AL1(0,0)                                                
         DC    AL2(QSQUEEZE-SUBSYSD,VALSQ-T60B39)                               
         DC    XL4'00'                                                          
*                                                                               
         DC    CL8'RIGHT'                                                       
         DC    AL1(2,1),AL1(DETIYN,0)                                           
         DC    AL2(QRIGHT-SUBSYSD,0)                                            
         DC    XL4'00'                                                          
*                                                                               
         DC    CL8'DETAIL'                                                      
         DC    AL1(3,MAXCATS*L'QDETAIL),AL1(0,0)                                
         DC    AL2(QDETAIL-SUBSYSD,VALCATG-T60B39)                              
         DC    XL4'00'                                                          
*                                                                               
         DC    CL8'WCODES'                                                      
         DC    AL1(2,MAXWORK*L'QWC),AL1(0,0)                                    
         DC    AL2(QWC-SUBSYSD,VALWC-T60B39)                                    
         DC    XL4'00'                                                          
*                                                                               
         DC    CL8'SWCODE'                                                      
         DC    AL1(2,2),AL1(0,0)                                                
         DC    AL2(QSW-SUBSYSD,VALSW-T60B39)                                    
         DC    XL4'00'                                                          
*                                                                               
         DC    CL8'DF'                                                          
         DC    AL1(2,1),AL1(0,0)                                                
         DC    AL2(QDRAFT-SUBSYSD,VALDFT-T60B39)                                
         DC    XL4'00'                                                          
*                                                                               
         DC    CL8'CROUND'                                                      
         DC    AL1(2,1),AL1(DETIYN,0)                                           
         DC    AL2(QCROUND-SUBSYSD,0)                                           
         DC    XL4'00'                                                          
*                                                                               
DETAILS  EQU   (*-DETTAB)/L'DETTAB                                              
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*              OTHER DSECTS ARE HIDDEN IN HERE                                  
         SPACE 3                                                                
*DDSPOOLD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
*DDSPLWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
*ACPROWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACPROWORKD                                                     
         PRINT ON                                                               
*DDMASTD                                                                        
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
*ACGENBOTH                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
*ACGENFILE                                                                      
*PREFIX=X                                                                       
       ++INCLUDE ACGENFILE                                                      
*PREFIX=                                                                        
         PRINT ON                                                               
*DDBIGBOX                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
*DDTWADCOND                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDTWADCOND                                                     
         PRINT ON                                                               
*ACJOBBERD                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACJOBBERD                                                      
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE ACPRO39COM                                                     
* DSECT TO COVER OPTIONS TABLE KEYWORDS                                         
*                                                                               
OPTTABD  DSECT                     **OPTIONS TABLE ENTRY**                      
OPTNAME  DS    CL8                 OPTION KEYWORD NAME                          
OPTMINL  DS    X                   MINIMUM INPUT LENGTH                         
OPTLEN   DS    X                   OUTPUT LENGTH                                
OPTIND   DS    X                   INDICATORS                                   
OPTIYES  EQU   X'80'               SET C'Y' IN VALUE FIELD                      
OPTIVAL  EQU   X'40'               VALUE IS REQUIRED                            
         DS    X                                                                
OPTDISP  DS    XL2                 DISPLACEMENT TO OUTPUT VALUE                 
OPTROUT  DS    XL2                 DISPLACEMENT TO PARM VALUE ROUTINE           
         DS    XL4                 SPARE                                        
OPTTABL  EQU   *-OPTTABD           OPTION TABLE ENTRY LENGTH                    
         SPACE 2                                                                
* DSECT TO COVER DETAIL TABLE KEYWORDS                                          
*                                                                               
DETTABD  DSECT                     **DETAIL TABLE ENTRY**                       
DETNAME  DS    CL8                 DETAIL KEYWORD NAME                          
DETMINL  DS    X                   MINIMUM INPUT LENGTH                         
DETLEN   DS    X                   OUTPUT LENGTH                                
DETIND   DS    X                   INDICATORS                                   
DETIYN   EQU   X'80'               VALIDATE FOR YES/NO PARM VALUE               
         DS    X                                                                
DETDISP  DS    XL2                 DISPLACEMENT TO OUTPUT VALUE                 
DETROUT  DS    XL2                 DISPLACEMENT TO PARM VALUE ROUTINE           
         DS    XL4                 SPARE                                        
DETTABL  EQU   *-DETTABD           DETAIL TABLE ENTRY LENGTH                    
         EJECT                                                                  
* DSECT TO COVER PARSNIP BLOCK                                                  
*                                                                               
       ++INCLUDE DDPARSNIPD                                                     
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'042ACPRO39   04/11/07'                                      
         END                                                                    
