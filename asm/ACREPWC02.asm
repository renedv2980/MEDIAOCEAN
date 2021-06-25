*          DATA SET ACREPWC02  AT LEVEL 002 AS OF 08/16/00                      
***********************************************************************         
*             QOPT1:' '= EVERYTHING                                   *         
*                  :'C'= ONLY NON-GROUPED CLIENTS                     *         
*                  :'G'= ONLY GROUPED CLIENTS                         *         
*             QOPT2:'Y'= DOWNLOAD                                     *         
*             QOPT8:   = WIGROUP RECORDS LEDGER (NEED FOR READING)    *         
***********************************************************************         
*PHASE ACWC02A,+0                                                               
*INCLUDE DLFLD                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'WORKING INVESTMENT#2 CLIENT GROUPING REPORT'                    
ACWC02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACWC**,R7,R9    BASE REGISTERS 11,9                          
         L     RA,0(R1)                                                         
         USING ACWORKD,RA          RA = A(GLOBAL W/S)                           
         LA    RC,SPACEND                                                       
         USING ACWCD,RC            RC = A(SAVE W/S)                             
         L     R8,VBIGPRNT                                                      
         USING BIGPRNTD,R8                                                      
*                                                                               
         CLI   MODE,RUNFRST        RUN FIRST                                    
         BE    RUNF                                                             
         CLI   MODE,REQFRST        REQUEST FIRST                                
         BE    REQF                                                             
         CLI   MODE,PROCLEVA       PROCESS LEVEL A                              
         BE    PLEVA                                                            
         CLI   MODE,REQLAST        REQUEST LAST                                 
         BE    REQL                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* RUN FIRST                                                           *         
***********************************************************************         
         SPACE 1                                                                
RUNF     DS    0H                                                               
         USING ACCRECD,RE                                                       
         LA    RE,IO                                                            
         MVC   DISP2,=Y(ACCRFST-ACCKEY)      SET DISP2                          
         DROP  RE                                                               
*                                                                               
         MVC   VTYPES(VTYPLNQ),ADCONS        RELOCATE ADDRESSES                 
*                                                                               
         L     R2,ABOXRC           SET UP BOX ROUTINE                           
         ST    RC,0(R2)                                                         
         L     R2,ABXHOOK                                                       
         ST    R2,HEADHOOK                                                      
         L     R2,VEXTRAS                                                       
         USING RUNXTRAD,R2                                                      
         L     R2,ADMASTC                                                       
         USING MASTD,R2                                                         
         L     R4,MCBXAREA                                                      
         ST    R4,ADBOX                                                         
         USING BOXD,R4                                                          
         MVC   BOXWIDTH,=F'198'    SET WIDTH FOR REPORT                         
         GOTO1 DATCON,DMCB,(5,0),(5,TODAY)                                      
*                                                                               
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* REQUEST FIRST                                                       *         
***********************************************************************         
         SPACE 1                                                                
REQF     DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         LA    RE,IO               RE=A(IO AREA)                                
         LA    RF,IOLNQ            RF=(LENGTH OF IO AREA)                       
         SR    R1,R1                                                            
         MVCL  RE,R0               CLEAR IO AREA                                
*                                                                               
         LA    RE,WISTR            RE=A(STORAGE AREA)                           
         LA    RF,WISTRLNQ         RF=(LENGTH OF STORAGE AREA)                  
         SR    R1,R1                                                            
         MVCL  RE,R0               CLEAR STORAGE AREA                           
*                                                                               
         USING BIND,R2                                                          
         L     R2,AGRPTAB          GROUP  TABLE                                 
         XC    BININ,BININ           CLEAR BIN TABLE                            
         DROP  R2                                                               
*                                                                               
         L     RF,ADWNBUF             RF=A(DOWNLOAD BUFFER)                     
         XC    0(L'DWNBUF,RF),0(RF)   CLEAR DOWNLOAD BUFFER                     
         XC    DWNSTAT,DWNSTAT        CLEAR DOWNSTAT BYTE                       
*                                                                               
         CLI   QOPT2,C'Y'          NO BOXES FOR DOWNLOADING                     
         BNE   REQF10                                                           
         MVI   RCSUBPRG,9          SET PRINT RTE AS INVALID                     
         GOTO1 ADWNL,DMCB,(RC),DWNINIT      INITIALIZE DOWNLOAD RTE             
*                                                                               
REQF10   BAS   RE,BLDGRP                    BUILD GROUP TABLE                   
*                                                                               
REQFX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PROCESS LEVEL A                                                     *         
***********************************************************************         
         SPACE 1                                                                
PLEVA    DS    0H                                                               
         USING ACTRECD,R2                                                       
         L     R2,ADHEIRA          R2=A(LEVAL A ACCOUNT RECORD)                 
         CLC   ACTKUNT(2),=C'SJ'   ONLY RUN FOR SJ                              
         BNE   PLEVAX                                                           
*                                                                               
         CLC   LSTCLI,ACTKACT      SAME CLIENT CODE AS BEFORE?                  
         BE    PLEVAX              IF IT IS SKIP PUTTING TO BIN TABLE           
*                                                                               
         USING GRPD,R3                                                          
         LA    R3,GRPWRK                                                        
         MVC   GRPWRK,SPACES                                                    
*                                                                               
         MVC   GRPCLT,ACTKACT      PUT CLIENT CODE IN TABLE                     
         USING NAMELD,R2                                                        
         L     R2,ADLVANAM         R2=A(CLIENT NAME)                            
         CLI   NAMEL,NAMELQ        X'20' - ELEMENT                              
         BE    *+6                                                              
         DC    H'0'                                                             
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q+1)                                                 
         EX    R1,*+4                                                           
         MVC   GRPCLTNM(0),NAMEREC PUT CLIENT NAME IN TABLE                     
*                                                                               
PLEVA10  MVC   LSTCLI,GRPCLT       SAVE CLIENT CODE FOR ABOVE COMPARE           
         GOTO1 BINADD,DMCB,GRPWRK,AGRPTAB     ADD TABLE ENTRY                   
*                                                                               
PLEVAX   B     EXIT                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
**********************************************************************          
* REQUEST LAST                                                       *          
**********************************************************************          
         SPACE 1                                                                
REQL     DS    0H                                                               
         CLI   QOPT2,C'Y'          WAS DOWNLOAD OPTION SELECTED?                
         BNE   REQL10                                                           
         GOTO1 ADWNRTE,DMCB,(RC)   IF SO - POINT TO DOWNLOAD                    
         B     REQLX                                                            
REQL10   BAS   RE,PRNT             NOT DOWNLOADING - PRINT                      
REQLX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* BUILD GROUP TABLE                                                   *         
***********************************************************************         
         SPACE 1                                                                
BLDGRP   NTR1                                                                   
         MVC   SVFLDS(SVLNQ),SPACES   CLEAR SAVED AREA                          
*                                                                               
         USING ACTRECD,R4                                                       
         LA    R4,SVKEY            ELSE - READ 2D LEDGER                        
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,RCCOMPFL    COMPANY CODE                                 
         MVI   ACTKUNT,C'F'                                                     
         MVC   ACTKLDG,QOPT8       LEDGER SPECIFIED IN REQUEST                  
         MVC   ACTKACT(2),QAPPL    READ ONLY FOR CAT SPECIFIED(IF ANY)          
         GOTO1 =A(DMHIGHDR),DMCB,(RC)       READ HIGH                           
         B     BLDG20                                                           
*                                                                               
BLDG10   GOTO1 =A(DMSEQDR),DMCB,(RC)                                            
BLDG20   CLC   SVKEY(ACTKACT-ACTKEY),IOKEY                                      
         BNE   BLDGX                                                            
         LA    R4,IO                                                            
         CLC   QAPPL,SPACES        ANY GROUP SPECIFIED?                         
         BE    BLDG25                                                           
         CLC   ACTKACT(4),QAPPL    DID WE GET BACK THE RIGHT RECORD?            
         BNE   BLDGX                                                            
         CLC   ACTKACT+4(3),SPACES  CAT ONLY IS ACCEPTABLE                      
         BE    BLDG25                                                           
         CLC   ACTKACT,QAPPL        FULL ACCOUNT MUST MATCH                     
         BNE   BLDGX                                                            
*                                                                               
BLDG25   CLC   ACTKACT,SPACES                                                   
         BNH   BLDG10                                                           
         GOTO1 =A(DMGETREC),DMCB,(RC)   GET RECORD                              
*                                                                               
         USING GRPD,R2                                                          
         LA    R2,GRPWRK           R2=A(GROUP TABLE WORK AREA)                  
         MVC   GRPWRK,SPACES                                                    
         MVC   SVGRP,ACTKACT       MOVE IN CODE TO BINTABLE                     
*                                                                               
         LA    R3,SVCATNM                                                       
         CLC   SVDIV,SPACES        ARE WE AT CAT LEV OR DIV LEV?                
         BE    *+8                                                              
         LA    R3,SVDIVNM                                                       
*                                                                               
         MVC   0(L'SVCATNM,R3),SPACES    CLEAR NAME FIELD                       
*                                                                               
         USING NAMELD,R5                                                        
         LR    R5,R4               R5=A(IO)                                     
         MVI   ELCODE,NAMELQ       X'20' - NAME ELEMENT                         
         BAS   RE,GETEL2                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q+1)                                                 
         EX    R1,*+4                                                           
         MVC   0(0,R3),NAMEREC     SAVE OFF NAME                                
*                                                                               
         USING FFTELD,R5                                                        
         LR    R5,R4               R5=A(IO)                                     
         MVI   ELCODE,FFTELQ       X'16' - FREEFORM TEXT ELEMENT                
         BAS   RE,GETEL2                                                        
         BNE   BLDG10                                                           
         CLI   FFTTYPE,FFTTCLIC    ARE THESE WORK GROUP ELEMENTS?               
         BNE   BLDG10                                                           
*                                                                               
         SR    R0,R0                                                            
         IC    R0,FFTDLEN          ACTUAL TEXT LENGTH                           
         LA    R3,FFTDATA          TEXT DATA - CLIENT CODES                     
*                                                                               
BLDG30   LTR   R0,R0                                                            
         BZ    BLDG10              END OF ELEMENT READ FOR NEXT                 
         MVC   GRPCLT,0(R3)        MOVE IN CLIENT CODES TO TABLE                
*                                                                               
         MVC   GRPCDE,SVGRP        AT DIV LEV FILL IN TABLE                     
         MVC   GRPCATNM,SVCATNM    CATEGORY NAME                                
         MVC   GRPDIVNM,SVDIVNM    DIVISION NAME                                
*                                                                               
         GOTO1 BINADD,DMCB,GRPWRK,AGRPTAB    ADD TO GROUP TABLE                 
*                                                                               
         LA    R3,L'GRPCLT(R3)     BUMP TO NEXT CLIENT CODE                     
         LA    R1,L'GRPCLT                                                      
         SR    R0,R1                                                            
         B     BLDG30                                                           
*                                                                               
BLDGX    B     EXIT                                                             
         DROP  R2,R4,R5                                                         
         EJECT                                                                  
**********************************************************************          
* PRINT INFORMATION FROM TABLE                                       *          
**********************************************************************          
         SPACE 1                                                                
PRNT     NTR1                                                                   
         MVI   RCSUBPRG,1                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
*                                                                               
         USING BIND,R1                                                          
         L     R1,AGRPTAB          R2=A(CLIENT TABLE)                           
         ICM   R0,15,BININ                                                      
         BZ    PRNTX                                                            
         USING GRPD,R2                                                          
         LA    R2,BINTAB                                                        
         DROP  R1                                                               
*                                                                               
         USING PLINED,R4                                                        
         LA    R4,XP                                                            
         MVC   XP,XSPACES                                                       
*                                                                               
PRNT10   CLI   QOPT1,C' '          DEFAULT IS EVERYTHING                        
         BE    PRNT20                                                           
*                                                                               
         LA    R1,X'80'            CONDITION EQUAL - NO GROUPED                 
         CLI   QOPT1,C'G'          GROUPED ONLY?                                
         BE    *+8                                                              
         LA    R1,X'70'            CONDITION NOT EQUAL - ONLY GROUPED           
*                                                                               
         CLC   GRPCDE,SPACES       IS THERE A GROUP CODE?                       
         EX    R1,*+4              MODIFY CODE TO BRANCH ACCORDINGLY            
         BC    0,PRNT30                                                         
*                                                                               
PRNT20   MVC   PALPHA,ALPHAID      AGENCY ID                                    
         MVC   PCLT,GRPCLT         CLIENT CODE                                  
         MVC   PDIV,GRPDIV         GROUP DIVISION CODE                          
         MVC   PDIVNM,GRPDIVNM     GROUP DIVISION NAME                          
         MVC   PCAT,GRPCAT         GROUP CATEGORY CODE                          
         MVC   PCATNM,GRPCATNM     GROUP CATEGORY NAME                          
         MVC   PDATE,TODAY         RUN DATE                                     
*                                                                               
         GOTO1 ACREPORT                                                         
PRNT30   LA    R2,GRPLNQ(R2)                                                    
         BCT   R0,PRNT10                                                        
*                                                                               
PRNTX    B     EXIT                                                             
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
* ADD ITEM TO BINSRCH TABLE AND ACCUMULATE TOTALS                     *         
*  P1    A(ITEM TO BE ADDED)                                          *         
*  P2    A(TABLE)                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING BIND,R5                                                          
BINADD   NTR1                                                                   
         L     R5,4(R1)                                                         
         MVC   DMCB+8(16),BININ    NUMBER LENGTH,KEY,MAX                        
         LA    R6,BINTAB           A(TABLE)                                     
         L     R3,0(R1)            A(ITEM)                                      
         GOTO1 BINSRCH,DMCB,(X'01',(R3)),(R6)                                   
         OC    DMCB(4),DMCB                                                     
         BNZ   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
         MVC   BININ,DMCB+8        UPDATE COUNT                                 
         CLI   DMCB,1              RECORD WAS ADDED                             
         BE    BINXIT                                                           
         L     R4,DMCB             A(RECORD FOUND)                              
         SR    R0,R0                                                            
         ICM   R0,1,BINNUM         NUMBER OF BUCKETS                            
         BZ    BINXIT                NO BUCKETS - EXIT                          
         DC    H'0'                NO BUCKETS SO DIE IF GETS HERE               
*        SR    R6,R6                                                            
*        IC    R6,BINFST           DISPLACEMENT TO FIRST BUCKET                 
*        AR    R4,R6               BUMP TO FIRST BUCKET IN TABLE                
*        AR    R3,R6               BUMP TO FIRST BUCKET IN NEW ITEM             
*INA10   AP    0(L'NCPBKT,R4),0(L'NCPBKT,R3)   ADD TO BUCKET                    
*        LA    R3,L'NCPBKT(R3)     BUMP TO NEXT ENTRY IN NEW ITEM               
*        LA    R4,L'NCPBKT(R4)     BUMP TO NEXT ENTRY IN TABLE                  
*        BCT   R0,BINA10                                                        
*                                                                               
BINXIT   B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* GETEL                                                               *         
***********************************************************************         
         SPACE 1                                                                
         GETEL R5,DATADISP,ELCODE                                               
         EJECT                                                                  
***********************************************************************         
* GETEL#2                                                             *         
***********************************************************************         
         SPACE 1                                                                
         GETELN R5,DISP2,ELCODE,2                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CONSTANTS                                                           *         
***********************************************************************         
         SPACE 1                                                                
ABOXRC   DC    A(BOXRC)                                                         
ABXHOOK  DC    A(BXHOOK)                                                        
ADUMP    DC    A(DUMP)             PRINTABLE ROUTINE                            
         EJECT                                                                  
***********************************************************************         
* RELOCATABLES                                                        *         
***********************************************************************         
         SPACE 1                                                                
ADCONS   DS    0F                                                               
         DC    A(DWNL)             DOWNLOAD ROUTINE                             
         DC    A(DWNBUF)           DOWNLOAD BUFFER                              
         DC    A(DWNRTE)           DOWNLOAD TOTALS ROUTINE                      
         DC    A(GRPTAB)           GROUP INFO BINTABLE                          
         DC    A(HD1TAB)           FIRST HEADLINE DWNLD FIELDS TABLE            
         DC    A(HD2TAB)           SECOND HEADLINE DWNLD FIELDS TABLE           
*                                                                               
         DC    V(PRNTBL)           PRINT DATA                                   
         DC    V(DLFLD)            DOWNLOAD MODULE                              
         EJECT                                                                  
***********************************************************************         
* HEADLINE TABLES                                                     *         
***********************************************************************         
         SPACE 1                                                                
HD1TAB   DS    0CL20               FIRST HEADLINE TABLE                         
         DC    CL20'LINE OF'                                                    
         DC    CL20'CLIENT'                                                     
         DC    CL20'GROUP LEVEL 2'                                              
         DC    CL20'GROUP LEVEL 2'                                              
         DC    CL20'GROUP LEVEL 1'                                              
         DC    CL20'GROUP LEVEL 1'                                              
         DC    CL20' '                                                          
HD1LNQ   EQU   (*-HD1TAB)/L'HD1TAB                                              
*                                                                               
HD2TAB   DS    0CL15               SECOND HEADLINE TABLE                        
         DC    CL15'BUSINESS'                                                   
         DC    CL15'CODE'                                                       
         DC    CL15'DIV CODE'                                                   
         DC    CL15'DIV NAME'                                                   
         DC    CL15'CAT CODE'                                                   
         DC    CL15'CAT NAME'                                                   
         DC    CL15'RUN DATE'                                                   
HD2LNQ   EQU   (*-HD2TAB)/L'HD2TAB                                              
         EJECT                                                                  
**********************************************************************          
* DOWNLOAD ROUTINE                                                   *          
**********************************************************************          
         SPACE 1                                                                
DWNRTE   DS    0D                                                               
         NMOD1 0,**DWNR**                                                       
         L     RC,0(R1)                                                         
         MVI   RCSUBPRG,9                                                       
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         USING BOXD,R3                                                          
         L     R3,ADBOX                                                         
         MVI   BOXOFF,C'Y'         NO BOXES IF DOWNLOADING                      
         DROP  R3                                                               
*                                                                               
*        TM    DWNSTAT,DWNHDLN     WERE THE HEADLINES DOWNLOADED?               
*        BO    *+8                                                              
*        BAS   RE,DWNHEAD          DOWNLOAD HEADLINES                           
*                                                                               
         USING BIND,R1                                                          
         L     R1,AGRPTAB          R2=A(CLIENT TABLE)                           
         ICM   R0,15,BININ                                                      
         BZ    DWNXIT                                                           
         USING GRPD,R2                                                          
         LA    R2,BINTAB                                                        
         DROP  R1                                                               
*                                                                               
DWNR10   CLI   QOPT1,C' '          DEFAULT IS EVERYTHING                        
         BE    DWNR20                                                           
*                                                                               
         LA    R1,X'80'            CONDITION EQUAL - NO GROUPED                 
         CLI   QOPT1,C'G'          GROUPED ONLY?                                
         BE    *+8                                                              
         LA    R1,X'70'            CONDITION NOT EQUAL - ONLY GROUPED           
*                                                                               
         CLC   GRPCDE,SPACES       IS THERE A GROUP CODE?                       
         EX    R1,*+4              MODIFY CODE TO BRANCH ACCORDINGLY            
         BC    0,DWNR30                                                         
*                                                                               
DWNR20   MVI   PRTSIZE,0                  SET COLUMN LEN=0-NO PADDING           
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'ALPHAID),ALPHAID  MOVE AGENCY ID TO DWN FLD             
         LA    R1,L'ALPHAID               ALPHAID LENGTH                        
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'GRPCLT),GRPCLT    MOVE CLIENT CODE TO DWN FLD           
         LA    R1,L'GRPCLT                PAD CLIENT COLUMN                     
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
         MVI   PRTSIZE,0                  RESET LENGTH=0-NO PADDING             
*                                                                               
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'GRPDIV),GRPDIV    MOVE DIV CODE TO DWN FLD              
         LA    R1,L'GRPDIV                GROUP DIVISION LENGTH                 
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD,GRPDIVNM            MOVE DIV NAME TO DWN FLD              
         LA    R1,L'GRPDIVNM              PAD DIVISION NAME COLUMN              
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
         MVI   PRTSIZE,0                  RESET LENGTH=0-NO PADDING             
*                                                                               
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'GRPCAT),GRPCAT    MOVE CAT CODE TO DWN FLD              
         LA    R1,L'GRPCAT                GROUP CATEGORY LENGTH                 
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD,GRPCATNM            MOVE CAT NAME TO DWN FLD              
         LA    R1,L'GRPCATNM              PAD CATEGORY NAME COLUMN              
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
         MVI   PRTSIZE,0                  RESET LENGTH=0-NO PADDING             
*                                                                               
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'TODAY),TODAY      MOVE RUN DATE TO DWN FLD              
         LA    R1,L'TODAY                 RUN DATE FIELD LENGTH                 
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         GOTO1 ADWNL,DMCB,(RC),DWNEOL     DOWNLOAD EOL MARKER                   
*                                                                               
DWNR30   LA    R2,GRPLNQ(R2)                                                    
         BCT   R0,DWNR10                                                        
*                                                                               
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         GOTO1 ADWNL,DMCB,(RC),DWNEOR     DOWNLOAD EOR MARKER                   
*                                                                               
DWNXIT   XMOD1                                                                  
         DROP  R2                                                               
                                                                                
         EJECT                                                                  
**********************************************************************          
* DOWNLOAD HEADLINES (ONCE PER REPORT)                               *          
**********************************************************************          
         SPACE 1                                                                
DWNHEAD  NTR1                                                                   
         OI    DWNSTAT,DWNHDLN     SET SWITCH TO SHOW HDLNS WERE DWNLD          
*                                                                               
         LA    R0,HD1LNQ                 NUMBER OF HEADINGS IN LINE 1           
         L     R2,AHD1TAB                FIRST HEADLINE TABLE                   
DWNH10   MVC   DWNFLD,SPACES             SPACE OUT DOWNLOAD FIELD               
         MVC   DWNFLD(L'HD1TAB),0(R2)    FIRST HEADLINE FIELDS                  
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT   DOWNLOAD TEXT                          
         LA    R2,L'HD1TAB(R2)                                                  
         BCT   R0,DWNH10                                                        
*                                                                               
         MVC   DWNFLD,SPACES             SPACE OUT DOWNLOAD FIELD               
         GOTO1 ADWNL,DMCB,(RC),DWNEOL    DOWNLOAD EOL MARKER                    
*                                                                               
         LA    R0,HD2LNQ                 NUMBER OF HEADINGS IN LINE 2           
         L     R2,AHD2TAB                SECOND HEADLINE TABLE                  
DWNH20   MVC   DWNFLD,SPACES             SPACE OUT DOWNLOAD FIELD               
         MVC   DWNFLD(L'HD2TAB),0(R2)    FIRST HEADLINE FIELDS                  
*                                                                               
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT   DOWNLOAD TEXT                          
         LA    R2,L'HD2TAB(R2)                                                  
         BCT   R0,DWNH20                                                        
*                                                                               
         MVC   DWNFLD,SPACES             SPACE OUT DOWNLOAD FIELD               
         GOTO1 ADWNL,DMCB,(RC),DWNEOL    DOWNLOAD EOL MARKER                    
*                                                                               
DWNHX    B     DWNXIT                                                           
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* DOWNLOAD MODULE                                                    *          
*          PARM1 - RC                                                *          
*          PARM2 - ACTION                                            *          
**********************************************************************          
         SPACE 1                                                                
DWNL     DS    0D                                                               
         NMOD1 0,**DOWN**                                                       
         L     RC,0(R1)                                                         
         L     RF,4(R1)                                                         
         STC   RF,DWNMODE          SAVE CURRENT MODE                            
         USING DLCBD,R5                                                         
         L     R5,ADWNBUF                                                       
*                                                                               
         CLI   DWNMODE,DWNINIT     INITIALIZE                                   
         BE    DWNL10                                                           
         CLI   DWNMODE,DWNTEXT     DOWN-LOAD TEXT                               
         BE    DWNL20                                                           
         CLI   DWNMODE,DWNNUM      DOWN-LOAD NUMBER                             
         BE    DWNL30                                                           
         CLI   DWNMODE,DWNPACK     DOWN-LOAD NUMBER (PACKED)                    
         BE    DWNL40                                                           
         MVI   DLCBACT,DLCBEOL                                                  
         CLI   DWNMODE,DWNEOL      END OF LINE                                  
         BE    DWNL50                                                           
         MVI   DLCBACT,DLCBEOR                                                  
         CLI   DWNMODE,DWNEOR      END OF REPORT                                
         BE    DWNL50                                                           
         DC    H'0'                                                             
*                                                                               
* INITIALIZATION                                                                
*                                                                               
DWNL10   TM    DWNSTAT,DWNINTZ     HAS IT ALREADY BEEN INITIALIZED?             
         BO    DWNLX               YES - EXIT                                   
         MVI   DLCBACT,DLCBINIT    DOWN LOAD ACTION IS START                    
         LA    RE,XP               PRINT LINE                                   
         ST    RE,DLCBAPL                                                       
         LA    RE,DWNHOOK          POINT TO HOOK FOR APPLICATION                
         ST    RE,DLCBAPR                                                       
         MVC   DLCXMAXL,=Y(L'XP)                                                
         MVI   DLCXDELC,C' '       DELIMITER                                    
         MVI   DLCXEOTC,C'"'       TEXT DELIMITER                               
         MVI   DLCXEOTA,C''''      ALTERNATE TEXT DELIMITER                     
         MVI   DLCXEOLC,X'5E'      SEMI-COLON, END-OF-LINE                      
         MVI   DLCXEORC,C':'       END-OF-REPORT                                
         GOTO1 DLFLD,(R5)                                                       
         MVI   FORCEHED,C'Y'       EXCEPT FIRST TIME IN                         
         GOTO1 ACREPORT                                                         
         MVC   DLCBFLD,SPACES      MUST CLEAR FIRST TIME IN                     
*                                  TURN OFF DOWN-LOAD ROW FLDS AS C' '          
         OI    DWNSTAT,DWNINTZ     TURN ON INITIALIZED BYTE                     
         B     DWNLX               EXIT                                         
*                                                                               
* DOWNLOAD A RECORD - TEXT                                                      
*                                                                               
DWNL20   MVC   DLCBLEN,PRTSIZE     LEN OF FIELD-SET TO 0 (NO PADDING)           
         MVI   DLCBACT,DLCBPUT     ACTION IS PUT                                
         MVI   DLCBTYP,DLCBTXT     TYPE   IS TEXT                               
         MVC   DLCBFLD,SPACES                                                   
         MVC   DLCBFLD(L'DWNFLD),DWNFLD                                         
         B     DWNL50              DOWN-LOAD FIELD                              
*                                                                               
* DOWNLOAD A RECORD - NUMBER                                                    
*                                                                               
DWNL30   MVI   DLCBLEN,0           LEN OF FIELD-SET TO 0 (NO PADDING)           
         OI    DLCBFLG1,DLCBFTRM   DOWN-LOAD WITH TRAILING MINUS                
         MVI   DLCBACT,DLCBPUT     ACTION IS PUT                                
         MVI   DLCBTYP,DLCBNUM     TYPE   IS NUMBER                             
         MVI   DLCBLEN,16          YES, USE MAXIMUM LENGTH OF NUMERICS          
         MVC   DLCBFLD,SPACES                                                   
         MVC   DLCBFLD(L'DWNFLD),DWNFLD                                         
         B     DWNL50              DOWN-LOAD FIELD                              
*                                                                               
* DOWNLOAD A RECORD - NUMBER (PACKED)                                           
*                                                                               
DWNL40   MVI   DLCBTYP,DLCBPACF    PACKED DATA                                  
         OI    DLCBFLG1,DLCBFTRM   DOWN-LOAD WITH TRAILING MINUS                
         MVI   DLCBACT,DLCBPUT     ACTION IS PUT                                
         MVI   DLCBLEN,L'PKFLDS    YES, USE MAXIMUM LENGTH OF NUMERICS          
         XC    DLCBFLD,DLCBFLD     CLEAN DWNLOAD FIELD TO 0'S                   
         MVC   DLCBFLD(L'DWNFLD),DWNFLD                                         
         NC    DLCBFLD,DLCBFLD     YES, MAKE SURE NUMERIC FLD NOT ZEROS         
         BNZ   DWNL50              NOT  ZERO, DOWN-LOAD FIELD                   
         MVI   DLCBLEN,1           ZERO, SET LENGTH 1 TO DOWN-LOAD A 0          
*                                                                               
* END OF LINE/END OF RECORD                                                     
*                                                                               
DWNL50   GOTO1 DLFLD,(R5)          DOWN-LOAD FIELD                              
*                                                                               
DWNLX    XMOD1                                                                  
         DROP  R5                                                               
         EJECT                                                                  
**********************************************************************          
* DOWNLOAD HOOK                                                      *          
**********************************************************************          
         SPACE 1                                                                
DWNHOOK  MVI   FORCEHED,C'N'       NEVER HEAD UP                                
         MVI   SPACING,1                                                        
         MVI   LINE,1                                                           
         L     RF,ACREPORT                                                      
         BR    RF                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* DATAMGR INTERFACE                                                  *          
**********************************************************************          
         SPACE 1                                                                
DMSEQDR  NMOD1 0,SEQ               READ SEQUENTIAL                              
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,DMRSEQ,=C'ACCDIR ',SVKEY,IO,0                       
         B     DMX                                                              
*                                                                               
DMHIGHDR NMOD1 0,HIGH              READ HIGH                                    
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,DMRDHI,=C'ACCDIR ',SVKEY,IO,0                       
         B     DMX                                                              
*                                                                               
DMREADDR NMOD1 0,READ              READ                                         
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,DMREAD,=C'ACCDIR ',SVKEY,IO,0                       
         B     DMX                                                              
*                                                                               
DMGETREC NMOD1 0,GREC              GET RECORD                                   
         L     RC,0(R1)            RESET RC                                     
         USING ACCRECD,R3                                                       
         LA    R3,IO                                                            
         GOTO1 DATAMGR,DMCB,DMGET,=C'ACCMST ',ACCKDA,IO,DMWORK                  
DMX      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BOX HOOK                                                            *         
***********************************************************************         
         SPACE 1                                                                
         USING BIGPRNTD,R8                                                      
         USING BOXD,R4                                                          
BXHOOK   DS    0D                                                               
         NMOD1 0,*BHOOK                                                         
         L     RC,BOXRC            RESTORE REG C                                
         L     R8,VBIGPRNT                                                      
         L     R4,ADBOX                                                         
*                                                                               
         CLI   RCSUBPRG,1          REGULAR P/O                                  
         BNE   BXXIT                                                            
*                                                                               
         MVC   BOXCOLS(165),XSPACES                                             
         MVC   BOXROWS,XSPACES                                                  
         MVI   BOXROWS+4,C'T'                                                   
         MVI   BOXROWS+8,C'M'                                                   
         MVI   BOXCOLS+1,C'L'                                                   
*                                                                               
         MVI   BOXCOLS+(PCLT-PRTLNE-1),C'C'                                     
         MVI   BOXCOLS+(PDIV-PRTLNE-1),C'C'                                     
         MVI   BOXCOLS+(PDIVNM-PRTLNE-1),C'C'                                   
         MVI   BOXCOLS+(PCAT-PRTLNE-1),C'C'                                     
         MVI   BOXCOLS+(PCATNM-PRTLNE-1),C'C'                                   
         MVI   BOXCOLS+(PDATE-PRTLNE-1),C'C'                                    
         MVI   BOXCOLS+PRLNQ,C'R'                                               
*                                                                               
BXXIT    MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXBLANK,C'N'                                                    
*                                                                               
         XMOD1 1                                                                
*                                                                               
BOXRC    DC    A(0)                                                             
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DUMP RECORDS                                                        *         
***********************************************************************         
         SPACE 1                                                                
DUMP     NMOD1 0,**DMP**                                                        
         L     RC,0(R1)                                                         
         CLI   QOPT7,C'Y'                                                       
         BNE   DUMPX                                                            
         LA    R0,L'MSG                                                         
         LA    R2,MSG                                                           
         L     R3,4(R1)                                                         
         L     R4,8(R1)                                                         
*                                                                               
         LA    R5,=C'2D'                                                        
         GOTO1 PRNTBL,DMCB,((R0),(R2)),(R3),C'DUMP',(R4),(R5),         X        
               (C'P',PRINT)                                                     
*                                                                               
DUMPX    XIT1                                                                   
*        MVC   MSG,=CL10'GROUP REC'                                             
*        GOTO1 ADUMP,DMCB,(RC),(R2),NCPLNQ                                      
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BUFFERS                                                             *         
***********************************************************************         
         SPACE 1                                                                
*                                                                               
* BINTABLE 1 - GROUP TABLE                                                      
*                                                                               
         DC    C'**GROUP**'                                                     
GRPTAB   DS    0D                  BINTABLE CONSTANTS FOR TABLE 1               
         DC    F'0'                    NUMBER IN TABLE                          
         DC    AL4(GRPLNQ)             LENGTH OF ENTRY                          
         DC    AL1(0)                  DISP. TO KEY                             
         DC    AL3(GRPKLNQ)            KEY LENGTH                               
         DC    AL4(GRPMAX)             MAX IN TABLE                             
         DC    AL1(0)                  NUMBER OF BUCKETS - NO BUCKETS           
         DC    AL1(0)                  DISPLACEMENT TO FIRST BUCKET             
         DS    (GRPMAX*GRPLNQ)XL1      TABLE                                    
*                                                                               
GRPMAX   EQU   4000                                                             
*                                                                               
         DS    0D                  DOWNLOAD BUFFER                              
DWNBUF   DS    CL250                                                            
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE                                                     *         
***********************************************************************         
         SPACE 1                                                                
ACWCD    DSECT                                                                  
ADBOX    DS    A                   ADDRESS OF BOX ROUTINE                       
*                                                                               
VTYPES   DS    0A                                                               
ADWNL    DS    A                   DOWNLOAD ROUTINE                             
ADWNBUF  DS    A                   DOWNLOAD BUFFER                              
ADWNRTE  DS    A                   DOWNLOAD TOTALS ROUTINE                      
AGRPTAB  DS    A                   GROUP INFO BINTABLE                          
AHD1TAB  DS    A                   FIRST HEADLINE DWNLD FIELDS TABLE            
AHD2TAB  DS    A                   SECOND HEADLINE DWNLD FIELDS TABLE           
*                                                                               
PRNTBL   DS    V                   PRINT DATA                                   
DLFLD    DS    V                   DOWNLOAD MODULE                              
VTYPLNQ  EQU   *-VTYPES                                                         
*                                                                               
DISP2    DS    H                   DISPLACEMENT TO ELEMENTS                     
TODAY    DS    CL8                 TODAY'S DATE MMMDD/YY                        
*                                                                               
WISTR    DS    0C                  STORAGE TO BE CLEARED WITH EACH REQ          
*                                                                               
* THE FOLLOWING FIELDS (LSTXXX) MUST MATCH SORT RECORD (NCPD)                   
*                                                                               
LSTGRP   DS    0CL7                LAST GROUP                                   
LSTCAT   DS    CL4                 LAST CATEGORY                                
LSTDIV   DS    CL3                 LAST DIVISION                                
LSTCLI   DS    CL3                 LAST CLIENT                                  
*                                                                               
PKFLDS   DS    PL8                 PACKED FIELDS FOR CALCULATIONS               
*                                                                               
SVKEY    DS    CL49                SAVED AREA FOR DATAMANAGER CALLS             
*                                                                               
SVFLDS   DS    0CL1                                                             
SVGRP    DS    0CL7                SAVED AREA FOR GROUP CODE                    
SVCAT    DS    CL4                 SAVED AREA FOR CATEGORY CODE                 
SVDIV    DS    CL3                 SAVED AREA FOR DIVISION CODE                 
SVCATNM  DS    CL36                SAVED AREA FOR GROUP CATEGORY NAME           
SVDIVNM  DS    CL36                SAVED AREA FOR GROUP DIVISION NAME           
SVLNQ    EQU   *-SVFLDS                                                         
*                                                                               
DWNFLD   DS    CL36                SAVED AREA FOR FIELD TO BE DWNLOADED         
PRTSIZE  DS    CL1                 DOWNLOAD FLD PRINT SIZE FOR PADDING          
*                                                                               
DWNSTAT  DS    XL1                 DOWNLOAD STATUS                              
DWNINTZ  EQU   X'80'               DOWNLOAD INITIALIZED                         
DWNHDLN  EQU   X'40'               DOWNLOAD HEADLINES                           
*                                                                               
DWNMODE  DS    XL1                 DOWNLOAD MODE                                
DWNINIT  EQU   1                      DOWN-LOAD INITIALIZATION                  
DWNEOL   EQU   2                      MARK END OF LINE                          
DWNEOR   EQU   3                      MARK END OF REPORT                        
DWNTEXT  EQU   4                      DOWN-LOAD TEXT                            
DWNNUM   EQU   5                      DOWN-LOAD NUMBER                          
DWNPACK  EQU   6                      DOWN-LOAD NUMBER (PACKED)                 
*                                                                               
COMPLEN  DS    XL1                 COMPARE LENGTH                               
ELCODE   DS    XL1                 ELEMENT CODE FOR GETEL                       
*                                                                               
FLAG     DS    XL1                 SPECIFIC TO EACH REQUEST                     
*                                                                               
DWNHDOPT DS    XL1                 DOWNLOAD ROUTINES HEADING OPTION             
DWNRG    EQU   X'80'               REGULAR RUN                                  
DWNRGOFF EQU   X'40'               REGULAR RUN BY OFFICE                        
DWNAP    EQU   X'20'               A/P BREAKDOWN                                
DWNAPOFF EQU   X'10'               A/P BREAKDOWN BY OFFICE                      
DWNAR    EQU   X'08'               A/R BREAKDOWN                                
*                                                                               
MSG      DS    CL10                                                             
*                                                                               
DKEY     DS    CL(L'ACCKEY)        DIRECTORY KEY                                
DA       DS    F                   DISK ADDRESS                                 
*                                                                               
EOF      EQU   X'FF'               END OF FILE MARKER                           
*                                                                               
GRPWRK   DS    CL(GRPLNQ)          BINSEARCH WORK AREA - GROUP   TABLE          
WISTRLNQ EQU   *-WISTR             STORAGE LENGTH                               
*                                                                               
IO       DS    0CL2042                                                          
IOKEY    DS    CL42                KEY                                          
IODATA   DS    CL2000              DATA                                         
IOLNQ    EQU   *-IO                LENGTH                                       
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER GROUP TABLE                                          *         
***********************************************************************         
         SPACE 1                                                                
GRPD     DSECT                                                                  
GRPCLT   DS    CL3                 CLIENT CODE                                  
GRPKLNQ  EQU   *-GRPD              LENGTH OF KEY                                
GRPCDE   DS    0CL7                GROUP CODE                                   
GRPCAT   DS    CL4                 LEVEL 1 - CATEGORY                           
GRPDIV   DS    CL3                 LEVEL 2 - DIVISION                           
GRPCLTNM DS    CL36                CLIENT CODE    NAME                          
GRPCATNM DS    CL36                GROUP CATEGORY NAME                          
GRPDIVNM DS    CL36                GROUP DIVISION NAME                          
GRPLNQ   EQU   *-GRPD                                                           
         EJECT                                                                  
***********************************************************************         
* DSECT FOR BINSRCH PARAMETERS                                        *         
***********************************************************************         
         SPACE 1                                                                
BIND     DSECT                                                                  
BININ    DS    F                   NUMBER IN TABLE                              
BINLEN   DS    F                   RECORD LENGTH                                
BINDISP  DS    CL1                 DISPLACEMENT IN RECORD                       
BINKEY   DS    CL3                 KEY LENGTH                                   
BINMAX   DS    F                   MAXIMUM NUMBER IN TABLE                      
BINKLN   EQU   *-BIND                                                           
BINNUM   DS    CL1                 NUMBER OF BUCKETS                            
BINFST   DS    CL1                 DISPLACEMENT TO FIRST BUCKET                 
BINTAB   DS    0C                  THE TABLE                                    
         EJECT                                                                  
***********************************************************************         
* PRINT DESCT                                                         *         
***********************************************************************         
         SPACE 1                                                                
PLINED   DSECT                                                                  
PRTLNE   DS    0C                  PRINT LINE # CLIENT CODES                    
         DS    CL2                                                              
PALPHA   DS    CL2                 ALPHA ID                                     
         DS    CL5                                                              
PCLT     DS    CL3                 CLIENT CODE                                  
         DS    CL5                                                              
PDIV     DS    CL3                 GROUP DIVISION                               
         DS    CL5                                                              
PDIVNM   DS    CL36                GROUP DIVISION NAME                          
         DS    CL1                                                              
PCAT     DS    CL4                 GROUP CATEGORY                               
         DS    CL5                                                              
PCATNM   DS    CL36                GROUP CATEGORY NAME                          
         DS    CL1                                                              
PDATE    DS    CL8                 RUN DATE                                     
         DS    CL1                                                              
PRLNQ    EQU   *-PRTLNE                                                         
         EJECT                                                                  
***********************************************************************         
*              ++INCLUDES                                             *         
***********************************************************************         
         SPACE 1                                                                
* DDCNTRL                                                                       
*                                                                               
       ++INCLUDE DDCNTRL                                                        
*                                                                               
* DMWRKRK                                                                       
*                                                                               
       ++INCLUDE DMWRKRK                                                        
         EJECT                                                                  
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
* ACBIGPRINTD                                                                   
         PRINT OFF                                                              
       ++INCLUDE ACBIGPRNTD                                                     
       ++INCLUDE ACGENPOST                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACMASTD                                                        
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE DDLOGOD                                                        
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDBOXEQUS                                                      
       ++INCLUDE DDREPXTRAD                                                     
       ++INCLUDE DDDLCB                                                         
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002ACREPWC02 08/16/00'                                      
         END                                                                    
