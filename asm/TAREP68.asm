*          DATA SET TAREP68    AT LEVEL 003 AS OF 04/08/14                      
*PHASE T70368C,*                                                                
*INCLUDE DLFLD                                                                  
         TITLE 'T70368 - NY STATE WITHHOLDING DIFF'                             
T70368   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70368                                                         
         LR    RE,RC                                                            
         L     RC,0(R1)            RC=A(CONTROLLER W/S)                         
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(SCREEN)                                 
         USING T703FFD,RA                                                       
         L     R9,ASUBSYSD         R9=A(SYSTEM W/S)                             
         USING SUBSYSD,R9                                                       
         L     R8,ASPOOLD          R8=A(SPOOL DSECT)                            
         USING SPOOLD,R8                                                        
         LA    R7,BUFF                                                          
         LA    R7,8(R7)                                                         
         USING MYD,R7                                                           
         EJECT                                                                  
***********************************************************************         
*        MODE CONTROLLED ROUTINES                                     *         
***********************************************************************         
                                                                                
         GOTO1 INITIAL,DMCB,0      INITIALIZE                                   
                                                                                
MODE01   CLI   MODE,VALREC                                                      
         BNE   MODE05                                                           
                                                                                
         NI    GENSTAT3,X'FF'-DIEONERR  DON'T DIE ON ERRORS OFFLINE             
         LA    R2,BSMPERH          VALIDATE PERIOD                              
         CLI   5(R2),0                                                          
         JE    PERMISS             PERIOD MISSING                               
                                                                                
         LA    R3,BLOCK                                                         
         USING PERVALD,R3                                                       
         GOTO1 PDVAL,DMCB,(R3)     GO TO SYSTEM PERIOD VAL. ROUTINE             
         CLC   PVALNMNS,=H'3'      CAN'T REQUEST MORE THAN 3 MONTHS             
         BH    PERINV                                                           
         MVC   STDATE,PVALPSTA     SET PWOS DATES FOR SYSIO                     
         MVC   ENDATE,PVALPEND                                                  
                                                                                
         XC    STDATE,=X'FFFFFF'  COMPLEMENT IT                                 
         XC    ENDATE,=X'FFFFFF'  COMPLEMENT IT                                 
                                                                                
         LA    R2,BSMOPTH                                                       
         BAS   RE,VOPT             VALIDATE OPTIONS                             
                                                                                
         CLC   TAXUNIT,SPACES      MUST HAVE A TAX UNIT                         
         BNH   UNTMISS                                                          
                                                                                
         B     YES                                                              
                                                                                
MODE05   CLI   MODE,PRINTREP                                                    
         BNE   YES                                                              
                                                                                
         BAS   RE,PREP             PRINT REPORT                                 
                                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
                                                                                
UNTMISS  LAY   RF,UNTERR                                                        
         B     *+10                                                             
UNTINV   LAY   RF,UNTERR2                                                       
         B     *+10                                                             
PERMISS  LAY   RF,PERERR                                                        
         B     *+10                                                             
PERINV   LAY   RF,PERERR2                                                       
         MVC   CONHEAD(L'UNTERR),0(RF)                                          
         B     BADXIT                                                           
                                                                                
FLDINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         GOTO1 ERREX                                                            
                                                                                
BADXIT   MVI   ERROR,X'FE'                                                      
         GOTO1 ERREX2                                                           
                                                                                
                                                                                
UNTERR   DC    C'** ERROR ** MISSING UNIT=    '                                 
UNTERR2  DC    C'** ERROR ** INVALID UNIT     '                                 
PERERR   DC    C'** ERROR ** PERIOD MISSING   '                                 
PERERR2  DC    C'** ERROR ** PERIOD > 3 MONTHS'                                 
                                                                                
         GETEL R4,DATADISP,ELCODE                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*              ROUTINE TO VALIDATE OPTIONS                                      
***********************************************************************         
VOPT     NTR1                                                                   
         CLI   5(R2),0                                                          
         JE    UNTMISS                                                          
                                                                                
         LA    R3,BLOCK            R3=A(SCAN BLOCK)                             
         USING SCAND,R3                                                         
         GOTO1 SCANNER,DMCB,(R2),(X'80',(R3))                                   
         CLI   4(R1),0                                                          
         JE    FLDINV                                                           
         ZIC   R0,4(R1)            R0 = NUM OF SCAN BLOCK ENTRIES               
                                                                                
VOPT7    CLC   =C'UNIT',SCDATA1    TAX UNIT REQUESTED                           
         JNE   UNTMISS                                                          
         MVC   TAXUNIT,SCDATA2     SAVE UNIT                                    
         GOTO1 TAXVAL,DMCB,(3,TAXUNIT)                                          
         JNE   UNTINV              UNIT INVALID                                 
         CLC   TAXUNIT,=C'FD '     DON'T ALLOW FD, TOO MANY CHECKS              
         JE    UNTINV                                                           
                                                                                
         LA    R3,SCANNEXT         BUMP TO NEXT ELEMENT                         
         BCT   R0,VOPT7                                                         
                                                                                
VOPTX    J     XIT                                                              
         DROP  R3                  DROP THE SCAND DSECT                         
         EJECT                                                                  
***********************************************************************         
*        PRINT REPORT                                                 *         
***********************************************************************         
PREP     NTR1                                                                   
                                                                                
         LOAD  EP=TASEGUE,ERRET=LOADERR                                         
         ST    R0,TASEGUE                                                       
         XC    SEGBLK,SEGBLK                                                    
                                                                                
PREP10   MVC   SYSDIR,=CL8'CHKDIR'                                              
         MVC   SYSFIL,=CL8'CHKFIL'                                              
                                                                                
*        CLC   TAXUNIT,SPACES                                                   
*        BH    *+10                                                             
*        MVC   TAXUNIT,=C'NYC'     DEFAULT NYC IF NO UNIT REQUESTED             
                                                                                
         BAS   RE,INITDOWN         INITIALIZE DOWNLOAD                          
                                                                                
         USING TLCKPD,R3           USE PAYROLL YTD BY UNIT                      
         LA    R3,KEY                                                           
         XC    TLCKPKEY,TLCKPKEY                                                
         MVI   TLCKPCD,TLCKYCDQ    CHECKS BY PAYROLL YTD                        
         MVC   TLCKYEAR,=C'2011'    2011 YEAR                                   
         MVI   TLCKYCUR,C'U'       US$                                          
         MVC   TLCKYEMP,=C'PP '     DEFAULT                                     
                                                                                
PREP500  GOTO1 HIGH                                                             
                                                                                
PREP510  CLC   KEY(TLCKYEMP-TLCKPD),KEYSAVE  CHECKS BY PAYROLL YTD              
         JNE   PREP900                                                          
         MVC   CURREMP,TLCKYEMP                                                 
         MVC   CURRSSN,TLCKYSSN                                                 
                                                                                
         MVC   TLCKYTXU,TAXUNIT    NEW YORK STATE                               
         MVC   TLCKYDTE,ENDATE     UP TO DATE REQUESTED                         
         XC    TLCKYSEQ(7),TLCKYSEQ   CLEAR OUT REST TO FIND FIRST              
         GOTO1 HIGH                                                             
                                                                                
PREP530  CLI   TLCKPCD,TLCKYCDQ    ARE WE DONE WITH CHECKS?                     
         JNE   PREP900                                                          
         CLC   CURRSSN,TLCKYSSN                                                 
         BNE   PREP510                                                          
         CLC   TLCKYDTE,STDATE     STDATE-ENDATE ONLY                           
         BH    PREP800                                                          
                                                                                
         CLC   KEY(TLCKYDTE-TLCKPD),KEYSAVE  CHECKS BY PAYROLL YTD              
         JNE   PREP800                       FOR SSN & NY STATE                 
                                                                                
         MVI   FRSTCHK,C'N'                                                     
         XC    FRSTTAX,FRSTTAX                                                  
         MVC   SAVEKEY,KEY         SAVE READ SEQ                                
         GOTO1 GETREC              GET CHECK RECORD                             
                                                                                
         USING TACDD,R4            GET NY WITHHOLDING INFO                      
         MVC   CDCHK,SPACES                                                     
         L     R4,AIO                                                           
         MVI   ELCODE,TACDELQ                                                   
         BAS   RE,GETEL                                                         
         JNE   PREP549                                                          
         MVC   CDCHK,TACDCHK                                                    
         MVC   CDEARN,TACDEARN                                                  
         MVC   CDNET,TACDNET                                                    
         GOTO1 DATCON,DMCB,(1,TACDDTE),(20,CKDATE)                              
                                                                                
         USING TACWD,R4            GET NY WITHHOLDING INFO                      
PREP549  L     R4,AIO                                                           
         MVI   ELCODE,TACWELQ                                                   
         BAS   RE,GETEL                                                         
PREP550  JNE   PREP600                                                          
         CLC   TACWUNIT,TAXUNIT                                                 
         BE    PREP580                                                          
         BAS   RE,NEXTEL                                                        
         B     PREP550                                                          
                                                                                
PREP580  TM    TACWSTAT,TACWSWRK   HAS TO WORK IN REQUESTED UNIT                
         BZ    PREP790             NO, SKIP THIS CHECK                          
                                                                                
PREP585  CLI   TACWMST,C' '        IF NO MARRIAGE STATUS, SKIP                  
         BH    PREP590                                                          
         B     PREP790                                                          
                                                                                
PREP590  MVC   NYMSTAT,TACWMST     MARRIAGE STATUS                              
         MVC   NYEXS,TACWEXS       EXEMPTIONS                                   
         MVC   CWTAX,TACWTAX       GET TAX AMOUNT                               
         MVI   NYSTAT,C'N'                                                      
         TM    TACWSTAT,TACWSRES                                                
         BZ    *+8                                                              
         MVI   NYSTAT,C'Y'         RESIDENT                                     
                                                                                
         USING TACYD,R4            GET NY YTD                                   
PREP600  L     R4,AIO                                                           
         MVI   ELCODE,TACYELQ                                                   
         BAS   RE,GETEL                                                         
PREP650  JNE   PREP800                                                          
         CLC   TACYUNIT,TAXUNIT                                                 
         BE    PREP700                                                          
         BAS   RE,NEXTEL                                                        
         B     PREP650                                                          
                                                                                
PREP700  L     RF,TACYEARN         YTD NY TAXABLE WAGES                         
         ST    RF,NYWAGES          YTD NY TAXABLE WAGES BEFORE THIS             
         S     RF,CDEARN                                                        
         ST    RF,PREWAGES         PREV YTD WAGES                               
                                                                                
         MVC   NYTAX,TACYTAX       YTD NY TAX                                   
                                                                                
         USING TLW4D,R3                                                         
         MVC   SYSDIR,=CL8'TALDIR'                                              
         MVC   SYSFIL,=CL8'TALFIL'                                              
         XC    KEY,KEY             READ W4 FOR FIXED% TAX                       
         MVI   TLW4CD,TLW4CDQ                                                   
         MVC   TLW4SSN,CURRSSN                                                  
         GOTO1 HIGH                                                             
                                                                                
         CLC   KEY(TLW4LEN-TLW4D),KEYSAVE  MAKE SURE WE GOT W4                  
         BE    *+6                                                              
         DC    H'0'                HAS TO BE THERE                              
                                                                                
         GOTO1 GETREC                                                           
                                                                                
         USING TAW4D,R4            GET W4 DETAILS                               
         L     R4,AIO                                                           
         MVI   ELCODE,TAW4ELQ                                                   
         BAS   RE,GETEL                                                         
         JNE   PREP705                                                          
         MVC   W4NAM2,TAW4NAM2                                                  
         MVC   W4NAM1,TAW4NAM1                                                  
                                                                                
         USING TAWHD,R4            GET WITHHOLDING ELEMENT FOR NY               
PREP705  L     R4,AIO                                                           
         XC    WHFLAT,WHFLAT                                                    
         MVI   ELCODE,TAWHELQ                                                   
         BAS   RE,GETEL                                                         
PREP710  JNE   PREP750                                                          
         CLC   TAWHUNIT,TAXUNIT                                                 
         BE    PREP730                                                          
         BAS   RE,NEXTEL                                                        
         B     PREP710                                                          
                                                                                
PREP730  MVC   WHFLAT,TAWHFLAT                                                  
                                                                                
         USING TLCKPD,R3           USE PAYROLL YTD BY UNIT                      
PREP750  MVC   SYSDIR,=CL8'CHKDIR'                                              
         MVC   SYSFIL,=CL8'CHKFIL'                                              
         MVC   KEY,SAVEKEY         RESTORE READ SEQUENCE                        
         GOTO1 HIGH                                                             
                                                                                
         BAS   RE,ISFRSTCK         FIRST CHECK AFTER JUN 30?                    
                                                                                
         USING TASEGUED,R6                                                      
         LA    R6,SEGBLK                                                        
         XC    SEGBLK,SEGBLK       CLEAR OUT TASEGUE BLOCK                      
         MVC   TASEUNIT,TAXUNIT    NEW YORK STATE                               
         MVI   TASEFREQ,C'A'       ANNUAL TAX CALCULATION                       
         MVC   TASESTAT,NYMSTAT    MARRIAGE STATUS                              
         MVC   TASEEXS,NYEXS       EXEMPTIONS                                   
         MVC   TASERES,NYSTAT      RESIDENT                                     
         MVC   TASECKDT,CKDATE     CHECK DATE                                   
         MVC   TASEEMP,CURREMP     EMPLOYER                                     
         MVC   TASEEARN,NYWAGES    CHECK EARNING AS TAXABLE WAGES               
                                                                                
         GOTO1 TASEGUE,DMCB,(R6)   CALL TASEGUE AND PASS SEGUE BLOCK            
         MVC   NYTAXC,TASETAX      SAVE NY TAX CALCULATED                       
         MVC   C1TAX,TASETAX                                                    
                                                                                
         XC    PRETAXC,PRETAXC                                                  
         XC    C2TAX,C2TAX                                                      
         OC    WHFLAT,WHFLAT       IS THERE A FLAT TAX?                         
         BZ    PREP760             YES, USE THAT INSTEAD FOR CALC               
         SR    RE,RE                                                            
         L     RF,NYWAGES                                                       
         M     RE,WHFLAT                                                        
         D     RE,=F'5000'         PERCENTAGE                                   
         LTR   RF,RF                                                            
         BM    *+8                                                              
         AH    RF,=H'1'            & ROUND IT                                   
         SRA   RF,1                                                             
         ST    RF,C2TAX                                                         
         C     RF,NYTAXC           USE HIGHER TAX ALLTAX OR FIXED%              
         BL    PREP760                                                          
         ST    RF,NYTAXC                                                        
                                                                                
PREP760  DS    0H                                                               
         LA    R6,SEGBLK                                                        
         XC    SEGBLK,SEGBLK       CLEAR OUT TASEGUE BLOCK                      
         MVC   TASEUNIT,TAXUNIT    NEW YORK STATE                               
         MVI   TASEFREQ,C'A'       ANNUAL TAX CALCULATION                       
         MVC   TASESTAT,NYMSTAT    MARRIAGE STATUS                              
         MVC   TASEEXS,NYEXS       EXEMPTIONS                                   
         MVC   TASERES,NYSTAT      RESIDENT                                     
         MVC   TASECKDT,CKDATE     CHECK DATE                                   
         MVC   TASEEMP,CURREMP     EMPLOYER                                     
         MVC   TASEEARN,PREWAGES   PREV EARNING AS TAXABLE WAGES                
                                                                                
         GOTO1 TASEGUE,DMCB,(R6)   CALL TASEGUE AND PASS SEGUE BLOCK            
         MVC   PRETAXC,TASETAX     SAVE NY TAX CALCULATED                       
         MVC   P1TAX,TASETAX                                                    
                                                                                
         XC    P2TAX,P2TAX                                                      
         OC    WHFLAT,WHFLAT       IS THERE A FLAT TAX?                         
         BZ    PREP780             YES, USE THAT INSTEAD FOR CALC               
         SR    RE,RE                                                            
         L     RF,PREWAGES                                                      
         M     RE,WHFLAT                                                        
         D     RE,=F'5000'         PERCENTAGE                                   
         LTR   RF,RF                                                            
         BM    *+8                                                              
         AH    RF,=H'1'            & ROUND IT                                   
         SRA   RF,1                                                             
         ST    RF,P2TAX                                                         
         C     RF,PRETAXC          USE HIGHER TAX ALLTAX OR FIXED%              
         BL    PREP780                                                          
         ST    RF,PRETAXC                                                       
                                                                                
PREP780  DS    0H                                                               
         CLI   FRSTCHK,C'Y'                                                     
         BNE   PREP785                                                          
         MVC   PRETAXC,FRSTTAX     YTD AS OF JUN 30, USE AS PREV                
         MVC   P1TAX,FRSTTAX                                                    
         OC    WHFLAT,WHFLAT       IS THERE A FLAT TAX?                         
         BZ    PREP785             YES, USE THAT INSTEAD FOR CALC               
         MVC   P2TAX,FRSTTAX                                                    
                                                                                
PREP785  GOTO1 OUTPDOWN,DMCB,(C'T',CURRSSN),L'CURRSSN                           
         GOTO1 OUTPDOWN,DMCB,(C'T',W4NAM2),L'W4NAM2                             
         GOTO1 OUTPDOWN,DMCB,(C'T',W4NAM1),L'W4NAM1                             
         GOTO1 OUTPDOWN,DMCB,(C'T',NYMSTAT),L'NYMSTAT                           
         GOTO1 OUTPDOWN,DMCB,(C'T',NYEXS),L'NYEXS                               
         ICM   RF,15,WHFLAT                                                     
         EDIT  (RF),(12,TMPAMT),2,MINUS=YES,ALIGN=LEFT,ZERO=NOBLANK             
         GOTO1 OUTPDOWN,DMCB,(C'N',TMPAMT),12                                   
         GOTO1 OUTPDOWN,DMCB,(C'T',CKDATE),L'CKDATE                             
         GOTO1 OUTPDOWN,DMCB,(C'T',CDCHK),L'CDCHK                               
         ICM   RF,15,CDEARN                                                     
         EDIT  (RF),(12,TMPAMT),2,MINUS=YES,ALIGN=LEFT,ZERO=NOBLANK             
         GOTO1 OUTPDOWN,DMCB,(C'N',TMPAMT),12                                   
         ICM   RF,15,NYWAGES                                                    
         EDIT  (RF),(12,TMPAMT),2,MINUS=YES,ALIGN=LEFT,ZERO=NOBLANK             
         GOTO1 OUTPDOWN,DMCB,(C'N',TMPAMT),12                                   
         ICM   RF,15,CWTAX                                                      
         EDIT  (RF),(12,TMPAMT),2,MINUS=YES,ALIGN=LEFT,ZERO=NOBLANK             
         GOTO1 OUTPDOWN,DMCB,(C'N',TMPAMT),12                                   
*&&DO                                                                           
         ICM   RF,15,C1TAX                                                      
         EDIT  (RF),(12,TMPAMT),2,MINUS=YES,ALIGN=LEFT,ZERO=NOBLANK             
         GOTO1 OUTPDOWN,DMCB,(C'N',TMPAMT),12                                   
         ICM   RF,15,C2TAX                                                      
         EDIT  (RF),(12,TMPAMT),2,MINUS=YES,ALIGN=LEFT,ZERO=NOBLANK             
         GOTO1 OUTPDOWN,DMCB,(C'N',TMPAMT),12                                   
         ICM   RF,15,P1TAX                                                      
         EDIT  (RF),(12,TMPAMT),2,MINUS=YES,ALIGN=LEFT,ZERO=NOBLANK             
         GOTO1 OUTPDOWN,DMCB,(C'N',TMPAMT),12                                   
         ICM   RF,15,P2TAX                                                      
         EDIT  (RF),(12,TMPAMT),2,MINUS=YES,ALIGN=LEFT,ZERO=NOBLANK             
         GOTO1 OUTPDOWN,DMCB,(C'N',TMPAMT),12                                   
*&&                                                                             
         ICM   RF,15,NYTAXC        CURR TAX                                     
         S     RF,PRETAXC          PREV TAX                                     
         EDIT  (RF),(12,TMPAMT),2,MINUS=YES,ALIGN=LEFT,ZERO=NOBLANK             
         GOTO1 OUTPDOWN,DMCB,(C'N',TMPAMT),12                                   
         ICM   RF,15,CWTAX                                                      
         S     RF,NYTAXC                                                        
         A     RF,PRETAXC                                                       
         EDIT  (RF),(12,TMPAMT),2,MINUS=YES,ALIGN=LEFT,ZERO=NOBLANK             
         GOTO1 OUTPDOWN,DMCB,(C'N',TMPAMT),12                                   
         ICM   RF,15,CDNET                                                      
         EDIT  (RF),(12,TMPAMT),2,MINUS=YES,ALIGN=LEFT,ZERO=NOBLANK             
         GOTO1 OUTPDOWN,DMCB,(C'N',TMPAMT),12                                   
         BAS   RE,EOLDOWN                                                       
                                                                                
PREP790  GOTO1 SEQ                                                              
         J     PREP530                                                          
                                                                                
PREP800  MVC   TLCKYTXU,=X'FFFFFF'    BUMP TO NEXT SSN                          
         XC    TLCKYSEQ(10),TLCKYSEQ  CLEAR OUT REST                            
         J     PREP500                                                          
                                                                                
PREP900  BAS   RE,ENDDOWN          FINISH DOWNLOADABLE REPORT                   
         J     XIT                                                              
                                                                                
LOADERR  DC    H'0'    COULDN'T LOAD - R0=A(PHASE NAME), RF=RETURN CODE         
                                                                                
         EJECT                                                                  
***********************************************************************         
*        SEE IF FIRST CHECK AFTER JUNE 30                             *         
*              SETS FRSTCHK AND FRSTTAX                               *         
***********************************************************************         
                                                                                
ISFRSTCK NTR1                                                                   
         MVI   FRSTCHK,C'N'        INIT                                         
         XC    FRSTTAX,FRSTTAX                                                  
         GOTO1 SEQ                 SAVEKEY WAS SET BEFORE THIS ROUTINE          
                                                                                
         CLI   TLCKPCD,TLCKYCDQ    ARE WE DONE WITH CHECKS?                     
         JNE   IFCY                                                             
         CLC   CURRSSN,TLCKYSSN    SAME SSN                                     
         JNE   IFCY                                                             
         CLC   TLCKYDTE,STDATE     AFTER JUNE 30 CHECK                          
         BNH   IFCN                THIS IS NOT THE 1ST CHECK                    
                                                                                
         GOTO1 GETREC                                                           
                                                                                
         USING TACYD,R4            GET NY YTD                                   
         L     R4,AIO                                                           
         MVI   ELCODE,TACYELQ                                                   
         BAS   RE,GETEL                                                         
IFC100   JNE   IFCN                                                             
         CLC   TACYUNIT,TAXUNIT                                                 
         BE    IFC300                                                           
         BAS   RE,NEXTEL                                                        
         B     IFC100                                                           
                                                                                
IFC300   MVC   FRSTTAX,TACYTAX     YTD NY TAX                                   
IFCY     MVI   FRSTCHK,C'Y'                                                     
                                                                                
IFCN     MVC   KEY,SAVEKEY         RESTORE READ SEQ                             
         GOTO1 HIGH                                                             
         J     XIT                                                              
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
*        INITIALIZE DOWNLOAD SETTINGS                                 *         
*        ON ENTRY ... R5=A(DOWNLOAD BLOCK)                            *         
***********************************************************************         
                                                                                
INITDOWN NTR1                                                                   
         USING DLCBD,R5                                                         
         LA    R5,DLCB                                                          
         XC    DLCBD(DLCBXLX),DLCBD                                             
         MVI   DLCBACT,DLCBINIT    INITIALIZE FOR DOWNLOAD                      
         LA    R1,PRTDOWN          A(HOOK ROUTINE FOR PRINTING)                 
         ST    R1,DLCBAPR                                                       
         LA    R1,P                A(PRINT LINE)                                
         MVC   P,SPACES                                                         
         ST    R1,DLCBAPL                                                       
         MVC   DLCBAED,EDITOR                                                   
         MVC   DLCXMAXL,=Y(L'P)    MAXIMUM LENGTH OF PRINT LINE                 
         MVI   DLCXDELC,C' '       DELIMITER                                    
         MVI   DLCXEOTC,C'"'       TEXT DELIMITER                               
         MVI   DLCXEOTA,C''''      TEXT DELIMITER ALTERNATE                     
         MVI   DLCXEOLC,X'5E'      SEMI-COLON FOR END OF LINE                   
         MVI   DLCXEORC,C':'       END OF REPORT                                
         OI    DLCBFLG1,DLCBFXTN                                                
         GOTO1 =V(DLFLD),DLCBD                                                  
                                                                                
         MVC   TAXHEAD+6(3),TAXUNIT                                             
         MVC   CLCHEAD+11(3),TAXUNIT                                            
         MVC   NYEHEAD(3),TAXUNIT                                               
         MVC   YTDHEAD(3),TAXUNIT                                               
         MVC   NYTHEAD(3),TAXUNIT                                               
         MVC   NYCHEAD(3),TAXUNIT                                               
                                                                                
         GOTOR OUTPDOWN,DMCB,(C'T',SSNHEAD),L'SSNHEAD                           
         GOTOR OUTPDOWN,DMCB,(C'T',LNMHEAD),L'LNMHEAD                           
         GOTOR OUTPDOWN,DMCB,(C'T',FNMHEAD),L'FNMHEAD                           
         GOTOR OUTPDOWN,DMCB,(C'T',MSTHEAD),L'MSTHEAD                           
         GOTOR OUTPDOWN,DMCB,(C'T',EXPHEAD),L'EXPHEAD                           
         GOTOR OUTPDOWN,DMCB,(C'T',FXDHEAD),L'FXDHEAD                           
         GOTOR OUTPDOWN,DMCB,(C'T',CDTHEAD),L'CDTHEAD                           
         GOTOR OUTPDOWN,DMCB,(C'T',CHKHEAD),L'CHKHEAD                           
         GOTOR OUTPDOWN,DMCB,(C'T',NYEHEAD),L'NYEHEAD                           
         GOTOR OUTPDOWN,DMCB,(C'T',YTDHEAD),L'YTDHEAD                           
         GOTOR OUTPDOWN,DMCB,(C'T',NYTHEAD),L'NYTHEAD                           
*        GOTOR OUTPDOWN,DMCB,(C'T',CT1HEAD),L'CT1HEAD                           
*        GOTOR OUTPDOWN,DMCB,(C'T',CT2HEAD),L'CT2HEAD                           
*        GOTOR OUTPDOWN,DMCB,(C'T',PT1HEAD),L'PT1HEAD                           
*        GOTOR OUTPDOWN,DMCB,(C'T',PT2HEAD),L'PT2HEAD                           
         GOTOR OUTPDOWN,DMCB,(C'T',NYCHEAD),L'NYCHEAD                           
         GOTOR OUTPDOWN,DMCB,(C'T',DIFHEAD),L'DIFHEAD                           
         GOTOR OUTPDOWN,DMCB,(C'T',CNTHEAD),L'CNTHEAD                           
         BAS   RE,EOLDOWN                                                       
                                                                                
         J     XIT                                                              
                                                                                
***********************************************************************         
*        USER SUPPLIED PRINT ROUTINE                                  *         
***********************************************************************         
                                                                                
PRTDOWN  NTR1                                                                   
         MVI   LINE,1              PREVENT PAGE BREAK                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
         J     XIT                                                              
*                                                                               
***********************************************************************         
*        OUTPUT DOWNLOAD ENTRY                                        *         
*        ON ENTRY ... P1, B0    = TYPE TO PASS                        *         
*                         B1-B3 = ADDRESS OF DATA                     *         
*                     P2        = LENGTH                              *         
***********************************************************************         
                                                                                
OUTPDOWN NTR1                                                                   
         USING DLCBD,R5                                                         
         LA    R5,DLCB                                                          
         MVI   DLCBACT,DLCBPUT                                                  
         MVC   DLCBTYP,0(R1)                                                    
         OI    DLCBFLG1,DLCBFXFL                                                
                                                                                
         L     RF,0(R1)            RF=A(DOWNLOAD FIELD)                         
         L     RE,4(R1)            RE=L'DOWNLOAD FIELD                          
                                                                                
         LR    R1,RF                                                            
         AR    R1,RE                                                            
OPD10    SHI   R1,1                                                             
         CLI   0(R1),C' '                                                       
         JNE   OPD20                                                            
         BCT   RE,OPD10                                                         
         LHI   RE,1                                                             
OPD20    STC   RE,DLCBLEN                                                       
                                                                                
         BCTR  RE,0                                                             
         CLI   DLCBTYP,DLCBNUM     DATA TYPE IS NUMERIC                         
         BE    OPD30                                                            
                                                                                
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   DLCBFLX(0),0(RF)                                                 
         B     OPD50                                                            
                                                                                
OPD30    EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   DLCBFLD(0),0(RF)                                                 
                                                                                
OPD50    GOTO1 =V(DLFLD),DLCBD                                                  
         J     XIT                                                              
                                                                                
***********************************************************************         
*        FINISH LINE OF DOWNLOAD OUPUT                                *         
***********************************************************************         
                                                                                
EOLDOWN  NTR1                                                                   
         USING DLCBD,R5                                                         
         LA    R5,DLCB                                                          
         MVI   DLCBACT,DLCBEOL      END OF LINE                                 
         GOTO1 =V(DLFLD),DLCBD      (DON'T USE RF, BASR RE,RF BELOW)            
         J     XIT                                                              
                                                                                
***********************************************************************         
*        END DOWNLOAD                                                 *         
***********************************************************************         
                                                                                
ENDDOWN  NTR1                                                                   
         USING DLCBD,R5                                                         
         LA    R5,DLCB                                                          
         MVI   DLCBACT,DLCBEOR      END OF REPORT                               
         GOTO1 =V(DLFLD),DLCBD      LAST FOR REPORT                             
         J     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
AGYHEAD  DC    C'AGENCY'                                                        
COMHEAD  DC    C'COMMERCIAL'                                                    
VERHEAD  DC    C'VERSION'                                                       
NAMEHEAD DC    C'COMMERCIAL NAME'                                               
SSNHEAD  DC    C'PID'                                                           
LNMHEAD  DC    C'LAST NAME'                                                     
FNMHEAD  DC    C'FIRST NAME'                                                    
MSTHEAD  DC    C'MARITAL STATUS'                                                
EXPHEAD  DC    C'EXEMPTIONS'                                                    
FXDHEAD  DC    C'FIXED%'                                                        
ERNHEAD  DS    C'CHECK TAXABLE WAGES'                                           
TAXHEAD  DS    C'CHECK UNT TAX'                                                 
CLCHEAD  DS    C'CALCULATED UNT TAX'                                            
NYEHEAD  DC    C'UNT EARN'                                                      
YTDHEAD  DC    C'UNT YTD TAXABLE WAGES'                                         
NYTHEAD  DC    C'UNT TAX'                                                       
CT1HEAD  DC    C'CURR UNT ALLTAX'                                               
CT2HEAD  DC    C'CURR UNT FIXED TAX'                                            
PT1HEAD  DC    C'PREV UNT ALLTAX'                                               
PT2HEAD  DC    C'PREV UNT FIXED TAX'                                            
NYCHEAD  DC    C'UNT CALC'                                                      
DIFHEAD  DC    C'DIFF'                                                          
CNTHEAD  DC    C'CHECK NET'                                                     
CDTHEAD  DC    C'CHECK DATE'                                                    
CHKHEAD  DC    C'CHECK NUMBER'                                                  
                                                                                
*NDATE   DC    X'4EF8D6'           2011 JUL 29                                  
*TDATE   DC    X'4EFEFE'           2011 JAN 01                                  
*TDATE   DC    X'4EF8FE'           2011 JUL 01                                  
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        WORKING STORAGE                                                        
***********************************************************************         
                                                                                
MYD      DSECT                                                                  
TASEGUE  DS    A                   A(ALLTAX SEGUE ROUTINE)                      
                                                                                
FILTCAT  DS    CL3                 FILTER ON CATEGORY                           
SAVEKEY  DS    CL(L'TLCAKEY)                                                    
                                                                                
AGYCODE  DS    CL(L'TLCOAGY)       AGENCY CODE                                  
INVCODE  DS    CL6                 INVOICE CODE                                 
CKDATE   DS    CL8                 CHECK DATE                                   
CDCHK    DS    CL8                 CHECK NUMBER                                 
CURREMP  DS    CL3                 CURRENT EMPLOYER                             
CURRSSN  DS    CL9                 CURRENT SSN                                  
NYMSTAT  DS    CL1                 MARRIAGE STATUS                              
NYEXS    DS    CL2                 EXEMPTIONS                                   
NYSTAT   DS    CL1                 RESIDENCE (Y/N)                              
NYWAGES  DS    F                   NY TAXABLE WAGES                             
PREWAGES DS    F                   PREV YTD TAXABLE WAGES                       
NYTAX    DS    F                   ACTUAL NY TAX                                
NYTAXC   DS    F                   CALCULATED NY TAX                            
PRETAXC  DS    F                   PREVIOUS CALCULATED NY TAX                   
TMPAMT   DS    CL12                                                             
WHFLAT   DS    F                                                                
CDEARN   DS    F                                                                
CDNET    DS    F                                                                
CWTAX    DS    F                                                                
C1TAX    DS    F                                                                
C2TAX    DS    F                                                                
P1TAX    DS    F                                                                
P2TAX    DS    F                                                                
FRSTTAX  DS    F                   CHECK BEFORE STDATE TAX                      
FRSTCHK  DS    C                                                                
W4NAM2   DS    CL(L'TAW4NAM2)                                                   
W4NAM1   DS    CL(L'TAW4NAM1)                                                   
STDATE   DS    XL3                                                              
ENDATE   DS    XL3                                                              
TAXUNIT  DS    CL3                                                              
                                                                                
PROSTAT  DS    X                                                                
PSTRACE  EQU   X'80'                                                            
                                                                                
         DS    0F                                                               
SEGBLK   DS    CL(TASELQ)          TASEGUE BLOCK                                
                                                                                
         DS    0F                                                               
DLCB     DS    CL(DLCBXLX)         DOWNLOAD BLOCK                               
MYDLNQ   EQU   *-MYD                                                            
         EJECT                                                                  
       ++INCLUDE TAREPFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPCCD                                                       
         EJECT                                                                  
*DDGENTWA  (MUST FOLLOW LAST SCREEN)                                            
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*DDBIGBOX                                                                       
*TAGENFILE                                                                      
*DDPERVALD                                                                      
*TASYSDSECT                                                                     
*TASYSEQUS                                                                      
*DDDLCB                                                                         
*DDTWADCONS                                                                     
*TAREPWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE DDDLCB                                                         
       ++INCLUDE TASEGUED                                                       
TWADCON  EJECT                                                                  
       ++INCLUDE DDTWADCONS                                                     
       ++INCLUDE TAREPWORKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003TAREP68   04/08/14'                                      
         END                                                                    
